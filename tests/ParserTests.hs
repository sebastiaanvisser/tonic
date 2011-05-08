{-# LANGUAGE
    TypeOperators
  , OverloadedStrings
  , TypeOperators
  #-}
module Main where

import Control.Arrow
import Control.Arrow.List
import Control.Category
import Prelude hiding (concat, (.), id, elem)

import qualified Data.Text.Lazy as T

import Test.HUnit hiding (Node)
import Xml.Tonic

-- Run all tests.

main :: IO Counts
main = runTestTT $ TestList
  [ parseTextOnly
  , parseEmptyElems
  , parseFakeCloses
  , parseSingleElems
  , parseAttrs
  , parseNested
  ]

-------------------------------------------------------------------------------
-- Individual tests.

parseTextOnly :: Test
parseTextOnly = TestList
  [ parseTest ""      $ noXml
  , parseTest "   "   $ txt "   "
  , parseTest "t"     $ txt "t"
  , parseTest "  t"   $ txt "  t"
  , parseTest "t  "   $ txt "t  "
  , parseTest "\n t " $ txt "\n t "
  , parseTest "/  t"  $ txt "/  t"
  , parseTest ">  t"  $ txt ">  t"
  ]

parseEmptyElems :: Test
parseEmptyElems = TestList
  [ parseTest "<x/>"        $ empt "x"
  , parseTest "<x></x>"     $ empt "x"
  , parseTest "<x>"         $ empt "x"
  , parseTest "< x />"      $ empt "x"
  , parseTest "< x ></ x >" $ empt "x"
  , parseTest "< x >"       $ empt "x"
  , parseTest "<x/>a"       $ empt "x" <+> txt "a"
  , parseTest "<x></x>a"    $ empt "x" <+> txt "a"
  ]

parseFakeCloses :: Test
parseFakeCloses = TestList
  [ parseTest "</x>"    $ noXml
  , parseTest "</ x >"  $ noXml
  , parseTest "</x>a"   $ txt "a"
  , parseTest "</ x >a" $ txt "a"
  ]

parseSingleElems :: Test
parseSingleElems = TestList
  [ parseTest "<x> </x>"          $ chld "x" (txt " ")
  , parseTest "<x>a</x>"          $ chld "x" (txt "a")
  , parseTest "<x> a </x>"        $ chld "x" (txt " a ")
  , parseTest "<x>   </x>"        $ chld "x" (txt "   ")
  , parseTest "<y><x> </x></y>"   $ chld "y" (chld "x" (txt " "))
  , parseTest "<y><x>a</x></y>"   $ chld "y" (chld "x" (txt "a"))
  , parseTest "<y><x> a </x></y>" $ chld "y" (chld "x" (txt " a "))
  , parseTest "<y><x>   </x></y>" $ chld "y" (chld "x" (txt "   "))
  ]

parseAttrs :: Test
parseAttrs = TestList
  [ parseTest "<a href></a>"                      $ onlya "a" (av "href" "")
  , parseTest "<a href  ></a>"                    $ onlya "a" (av "href" "")
  , parseTest "<a href=\"b\"></a>"                $ onlya "a" (av "href" "b")
  , parseTest "<a href=\'b\'></a>"                $ onlya "a" (av "href" "b")
  , parseTest "<a href=b></a>"                    $ onlya "a" (av "href" "b")
  , parseTest "<a  href  =  \"b\"  ></a>"         $ onlya "a" (av "href" "b")
  , parseTest "<a  href  =  \'b\'  ></a>"         $ onlya "a" (av "href" "b")
  , parseTest "<a href=b  ></a>"                  $ onlya "a" (av "href" "b")
  , parseTest "<a href=\"a\" href=\"b\"/>"        $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a href=\"a\" href=\'b\'/>"        $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a href=\"a\" href=b/>"            $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a href=\"a\"  href  =  \"b\"  />" $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a href=\"a\"  href  =  \'b\'  />" $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a href=\"a\" href=b  />"          $ onlya "a" (av "href" "a" <+> av "href" "b")
  , parseTest "<a e f g/>"                        $ onlya "a" (av "e" "" <+> av "f" "" <+> av "g" "")
  ]

parseNested :: Test
parseNested = TestList
  [ parseTest "<a><b></b></a>"               $ chld "a" (empt "b")
  , parseTest "<a><b/></a>"                  $ chld "a" (empt "b")
  , parseTest "<a><b/>"                      $ chld "a" (empt "b")
  , parseTest "<a><b>"                       $ chld "a" (empt "b")
  , parseTest "<a> x <b></b> y </a>"         $ chld "a" (txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a> x <b/> y </a>"            $ chld "a" (txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a> x <b/> y "                $ chld "a" (txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a> x <b> y "                 $ chld "a" (txt " x " <+> chld "b" (txt " y "))
  , parseTest "<a><c>i</c><b></b></a>"       $ chld "a" (chld "c" (txt "i") <+> empt "b")
  , parseTest "<a><c>i</c><b/></a>"          $ chld "a" (chld "c" (txt "i") <+> empt "b")
  , parseTest "<a><c>i</c><b/>"              $ chld "a" (chld "c" (txt "i") <+> empt "b")
  , parseTest "<a><c>i</c><b>"               $ chld "a" (chld "c" (txt "i") <+> empt "b")
  , parseTest "<a><c>i</c> x <b></b> y </a>" $ chld "a" (chld "c" (txt "i") <+> txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a><c>i</c> x <b/> y </a>"    $ chld "a" (chld "c" (txt "i") <+> txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a><c>i</c> x <b/> y "        $ chld "a" (chld "c" (txt "i") <+> txt " x " <+> empt "b" <+> txt " y ")
  , parseTest "<a><c>i</c> x <b> y "         $ chld "a" (chld "c" (txt "i") <+> txt " x " <+> chld "b" (txt " y "))
  ]

-------------------------------------------------------------------------------
-- Helper functions.

parseTest :: T.Text -> () `ListArrow` Node -> Test
parseTest a b = TestCase $ assertEqual "parser" (destruct id a) (runListArrow b ())

empt :: T.Text -> ListArrow a Node
empt t = elemNode . emptyElem t

txt :: T.Text -> ListArrow a Node
txt t = textNode . mkText t

chld :: T.Text -> (ListArrow a Node) -> ListArrow a Node
chld t c = elemNode . mkElem t noAttributes c

av :: T.Text -> T.Text -> ListArrow a Attribute
av = mkAttrValue

onlya :: T.Text -> ListArrow a Attribute -> ListArrow a Node
onlya t c = elemNode . mkElem t c noXml

