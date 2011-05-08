{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module PrintParse where

import Control.Applicative
import Test.QuickCheck

import qualified Data.Text.Lazy as T

import Xml.Tonic (parser, printer)
import Xml.Tonic.Types

main :: IO ()
main =
  do qc printParseIsIdentity
     qc parsePrintIsIdentity

qc :: Testable p => p -> IO ()
qc = quickCheckWith stdArgs { maxSize = 15, maxSuccess = 1000 }

printParseIsIdentity :: Nodes -> Bool
printParseIsIdentity a = parser (printer (unNodes a)) == unNodes a

parsePrintIsIdentity :: XmlText -> Bool
parsePrintIsIdentity a = printer (parser (unXmlText a)) == unXmlText a

-------------------------------------------------------------------------------
-- Random generators.

newtype XmlText = XmlText { unXmlText :: T.Text }
  deriving Show

instance Arbitrary XmlText where
  arbitrary = XmlText . printer . unNodes <$> arbitrary

instance Arbitrary Text where
  arbitrary = Text <$> elements textElems

instance Arbitrary CData where
  arbitrary = CData <$> elements xmlishElems

instance Arbitrary Comment where
  arbitrary = Comment <$> elements xmlishElems

instance Arbitrary Doctype where
  arbitrary = Doctype <$> elements textElems

instance Arbitrary ProcessingInstruction where
  arbitrary = ProcessingInstruction <$> elements xmlishElems

instance Arbitrary Attribute where
  arbitrary = Attribute <$> elements keyElems <*> elements valElems

instance Arbitrary Element where
  arbitrary = Element
   <$> elements nameElems
   <*> arbitrary
   <*> (unNodes <$> arbitrary)

newtype NonText = NonText { unNonText :: Node }

instance Arbitrary NonText where
  arbitrary = NonText <$>
    frequency
      [ (1, ElementNode               <$> arbitrary)
      , (1, CDataNode                 <$> arbitrary)
      , (1, CommentNode               <$> arbitrary)
      , (1, DoctypeNode               <$> arbitrary)
      , (1, ProcessingInstructionNode <$> arbitrary)
      ]

newtype Nodes = Nodes { unNodes :: [Node] }
  deriving Show

instance Arbitrary Nodes where
  arbitrary = Nodes . concat <$>
    do a <- map unNonText <$> arbitrary
       b <- map TextNode  <$> arbitrary
       return (zipWith (\x y -> [x, y]) a b)

nameElems :: [T.Text]
nameElems = ["one", "two-words", "ünîcødé"]

textElems :: [T.Text]
textElems = ["  ", "n", "one", "two words", "€ ünîcødé spaß"]

xmlishElems :: [T.Text]
xmlishElems = textElems ++ ["<xml/>", "  >padded<  "]

keyElems :: [T.Text]
keyElems = ["one", "two-words"]

valElems :: [T.Text]
valElems = xmlishElems ++ ["quoted-text"]

