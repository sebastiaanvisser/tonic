{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
  , StandaloneDeriving
  #-}
module Xml.Tonic.Types where

import qualified Data.Text.Lazy as T

type Xml        = Nodes
type Nodes      = [Node]
type Attributes = [Attribute]

data Node
  = ElementNode               { elementNode               :: Element               }
  | TextNode                  { textNode                  :: Text                  }
  | CDataNode                 { cdataNode                 :: CData                 }
  | CommentNode               { commentNode               :: Comment               }
  | DoctypeNode               { doctypeNode               :: Doctype               }
  | ProcessingInstructionNode { processingInstructionNode :: ProcessingInstruction }
  deriving (Eq, Show, Ord)

data Element = Element
  { name       :: T.Text
  , attributes :: Attributes
  , children   :: Nodes
  } deriving (Eq, Show, Ord)

data Attribute = Attribute
  { key   :: T.Text
  , value :: T.Text
  } deriving (Eq, Show, Ord)

newtype Text = Text { text :: T.Text }
  deriving (Eq, Show, Ord)

newtype CData = CData { cdata :: T.Text }
  deriving (Eq, Show, Ord)

newtype Comment = Comment { comment :: T.Text }
  deriving (Eq, Show, Ord)

newtype Doctype = Doctype { doctype :: T.Text }
  deriving (Eq, Show, Ord)

newtype ProcessingInstruction = ProcessingInstruction { instruction :: T.Text }
  deriving (Eq, Show, Ord)

