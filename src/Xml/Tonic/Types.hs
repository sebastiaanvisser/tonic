{-# LANGUAGE
    OverloadedStrings
  , GADTs
  , EmptyDataDecls
  #-}
module Xml.Tonic.Types where

import Data.List
import Data.Text (Text, unpack, strip)

data Node
data Attr

data QName = QName Text Text

data Xml n where
  Elem :: QName -> Xml [Attr] -> Xml [Node] -> Xml Node
  Attr :: QName -> Text                     -> Xml Attr
  Text :: Text                              -> Xml Node
  Cmnt :: Text                              -> Xml Node
  Proc :: Text -> Text                      -> Xml Node 
  List :: [Xml x]                           -> Xml [x] 

instance Show QName where
  show (QName "" n) = "|" ++ unpack n ++ "|"
  show (QName ns n) = "|" ++ unpack ns ++ ":" ++ unpack n ++ "|"

instance Show (Xml n) where
  show (Elem n a c) = "<" ++ show n ++ showAttrList a ++ ">\n" ++ indent (show c) ++ "</" ++ show n ++ ">"
  show (Attr k v)   = show k ++ "=|" ++ unpack v ++ "|"
  show (Text t)     = "|" ++ unpack (strip t) ++ "|"
  show (Cmnt c)     = "<!-- |" ++ unpack c ++ "| -->"
  show (Proc p v)   = "<? |" ++ unpack p ++ "| |" ++ unpack v ++ "| ?>"
  show (List ns)    = intercalate "\n" (map show ns)

indent :: String -> String
indent = unlines . map ("  "++) . lines

showAttrList :: Xml [Attr] -> String
showAttrList (List as) = concatMap (" "++) (map show as)

