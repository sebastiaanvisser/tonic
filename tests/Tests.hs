{-# LANGUAGE
    TypeOperators
  , OverloadedStrings
  #-}
module Main where

-- import Control.Arrow
-- import Control.Arrow.ArrowList
-- import Control.Arrow.List
import Control.Category
import Prelude hiding (concat, (.), id, elem)
import System.Random

import Test.HUnit
import Xml.Tonic

main :: IO Counts
main = runTestTT parseTests

parseTests = TestList
  [ parseTextOnly
  , parseEmptyElems
  , parseSingleElems
  ]
  where

  parseTextOnly = TestList
    [ p ""      []
    , p "   "   [text "   "]
    , p "t"     [text "t"]
    , p "  t"   [text "  t"]
    , p "t  "   [text "t  "]
    , p "  t  " [text "  t  "]
    , p "/  t"  [text "/  t"]
    , p ">  t"  [text ">  t"]
    ]

  parseEmptyElems = TestList
    [ p "<x/>"             [empty "x"]
    , p "<x></x>"          [empty "x"]
    , p "<x>"              [empty "x"]
    , p "<  x  />"         [empty "x"]
    , p "<  x ><  /  x  >" [empty "x"]
    , p "<  x  ></  x  >"  [empty "x"]
    , p "<  x  >"          [empty "x"]
    ]

  parseSingleElems = TestList
    [ p "<x>a</x>"    [single "x" [text "a"]]
    , p "<x> a </x>"  [single "x" [text " a "]]
    ]

  p a b = TestCase (assertEqual "parser" (destruct id a) b)
  empty n = ElementNode (Element n [] [])
  single  n s = ElementNode (Element n [] s)
  text  t = TextNode (Text t)

