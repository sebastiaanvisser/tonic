{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
  , StandaloneDeriving
  #-}
module Xml.Tonic.Types where

import qualified Data.Text.Lazy as T

data Element = Element
  { name       :: T.Text
  , attributes :: Attributes
  , children   :: ChildNodes
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

newtype Doctype = Doctype { docType :: T.Text }
  deriving (Eq, Show, Ord)

newtype ProcessingInstruction = ProcessingInstruction { instruction :: T.Text }
  deriving (Eq, Show, Ord)

type ChildNodes = [Child]
type Attributes = [Attribute]

data Child
  = ElementChild               { elementChild               :: Element               }
  | TextChild                  { textChild                  :: Text                  }
  | CDataChild                 { cdataChild                 :: CData                 }
  | CommentChild               { commentChild               :: Comment               }
  | DoctypeChild               { doctypeChild               :: Doctype               }
  | ProcessingInstructionChild { processingInstructionChild :: ProcessingInstruction }
  deriving (Eq, Show, Ord)

