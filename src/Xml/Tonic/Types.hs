{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
  , StandaloneDeriving
  #-}
module Xml.Tonic.Types where

import qualified Data.Text.Lazy as T

-- | An XML value is just a list of nodes.

type Xml = [Node]

-- | An XML node can be either an element, a text node, a CData construct, a
-- comment or a processing instruction. Although attributes are strictly
-- speaking also XML nodes they are not included to prevent constructing
-- invalid XML structures.

data Node
  = ElementNode               { elementNode               :: Element               }
  | TextNode                  { textNode                  :: Text                  }
  | CDataNode                 { cdataNode                 :: CData                 }
  | CommentNode               { commentNode               :: Comment               }
  | DoctypeNode               { doctypeNode               :: Doctype               }
  | ProcessingInstructionNode { processingInstructionNode :: ProcessingInstruction }
  deriving (Eq, Show, Ord)

-- | An element has a name (qualified or unqualified), a list of attributes and
-- a list of child nodes.

data Element = Element
  { name       :: T.Text
  , attributes :: [Attribute]
  , children   :: [Node]
  } deriving (Eq, Show, Ord)

-- | An attribute has a key (qualified or unqualified), and a value.

data Attribute = Attribute
  { key   :: T.Text
  , value :: T.Text
  } deriving (Eq, Show, Ord)

-- | Text nodes with a simple text value. (TODO: what kind of escaping?)

newtype Text = Text { text :: T.Text }
  deriving (Eq, Show, Ord)

-- | A CData block containing the raw text value.

newtype CData = CData { cdata :: T.Text }
  deriving (Eq, Show, Ord)

-- | A comment node containing the content text.

newtype Comment = Comment { comment :: T.Text }
  deriving (Eq, Show, Ord)

-- | A doctype or entity declaration simply containing the content text. Further
-- parsing is required when more structure is needed.

newtype Doctype = Doctype { doctype :: T.Text }
  deriving (Eq, Show, Ord)

-- | A processing instruction simply containing the instruction text. Further
-- parsing is required when more structure is needed.

newtype ProcessingInstruction = ProcessingInstruction { instruction :: T.Text }
  deriving (Eq, Show, Ord)

