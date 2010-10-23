{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
  , StandaloneDeriving
  #-}
module Xml.Tonic.Types
(
-- * Xml datatype as a family of types encoded in a single GADT.
 Xml (..)

-- * Phantom types as index for the 'Xml' family.
, Name
, Node
, Attr
)
where

import Data.Text.Lazy

data Name
data Node
data Attr

data Xml n where
  Element               :: Xml Name -> Xml [Attr] -> Xml [Node] -> Xml Node
  Attribute             :: Xml Name -> Text                     -> Xml Attr
  Text                  :: Text                                 -> Xml Node
  CData                 :: Text                                 -> Xml Node
  Comment               :: Text                                 -> Xml Node
  ProcessingInstruction :: Text -> Text                         -> Xml Node 
  NodeSet               :: [Xml Node]                           -> Xml [Node] 
  AttributeList         :: [Xml Attr]                           -> Xml [Attr] 
  QualifiedName         :: Text                                 -> Xml Name

deriving instance Eq   (Xml n)
deriving instance Show (Xml n)

