{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
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

import Data.List
import Data.Text (Text, unpack, strip)

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
  QualifiedName         :: Text -> Text                         -> Xml Name

instance Show (Xml n) where
  show (Element               n a c) = "<" ++ show n ++ show a ++ ">\n" ++ indent (show c) ++ "</" ++ show n ++ ">"
  show (Attribute             k v  ) = show k ++ "=|" ++ unpack v ++ "|"
  show (Text                  t    ) = "c|" ++ unpack (strip t) ++ "|"
  show (CData                 t    ) = "t|" ++ unpack (strip t) ++ "|"
  show (Comment               c    ) = "<!-- |" ++ unpack c ++ "| -->"
  show (ProcessingInstruction p v  ) = "<? |" ++ unpack p ++ "| |" ++ unpack v ++ "| ?>"
  show (NodeSet               ns   ) = intercalate "\n" (map show ns)
  show (AttributeList         as   ) = concatMap (" "++) (map show as)
  show (QualifiedName         ns n ) = "|" ++ unpack ns ++ ":" ++ unpack n ++ "|"

indent :: String -> String
indent = unlines . map ("  "++) . lines

