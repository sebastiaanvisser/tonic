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
  Element               :: { name          :: Xml Name,  attributes :: Xml [Attr], children :: Xml [Node] } -> Xml Node
  Attribute             :: { key           :: Xml Name,  value      :: Text                               } -> Xml Attr
  Text                  :: { text          :: Text                                                        } -> Xml Node
  CData                 :: { cdata         :: Text                                                        } -> Xml Node
  Comment               :: { comment       :: Text                                                        } -> Xml Node
  Doctype               :: { docType       :: Text                                                        } -> Xml Node
  ProcessingInstruction :: { instruction   :: Text                                                        } -> Xml Node 
  NodeSet               :: { nodeSet       :: [Xml Node]                                                  } -> Xml [Node] 
  AttributeList         :: { attributeList :: [Xml Attr]                                                  } -> Xml [Attr] 
  QualifiedName         :: { qname         :: Text                                                        } -> Xml Name

deriving instance Eq   (Xml n)
deriving instance Show (Xml n)

