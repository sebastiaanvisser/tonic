{-# LANGUAGE OverloadedStrings, GADTs #-}
module Xml.Tonic.Print
(
-- * Top level pretty printers.

  pretty
, compact

-- * Text builders.

, compactBuilder
, prettyBuilder

-- * Primitive builders.

, element
, attribute
, comment
, processingIntruction
, attributeList
, qualifiedName
)
where

import Data.List
import Data.Monoid
import Data.Text.Lazy (Text, strip)
import Data.Text.Lazy.Builder
import Xml.Tonic.Types

compact :: Xml a -> Text
compact = toLazyText . compactBuilder

pretty :: Xml a -> Text
pretty = toLazyText . prettyBuilder

compactBuilder :: Xml a -> Builder
compactBuilder = b
  where b :: Xml a -> Builder
        b (Element               n a c) = mconcat [element n a, b c, "</", b n, ">"]
        b (Attribute             k v  ) = attribute k v
        b (Text                  t    ) = fromLazyText (strip t)
        b (CData                 d    ) = fromLazyText d
        b (Comment               c    ) = comment c
        b (ProcessingInstruction p v  ) = processingIntruction p v
        b (NodeSet               ns   ) = mconcat (map b ns)
        b (AttributeList         as   ) = attributeList as
        b (QualifiedName         n    ) = qualifiedName n


indent :: [Builder] -> [Builder]
indent = map (mappend "  ")

prettyBuilder :: Xml a -> Builder
prettyBuilder = mconcat . intersperse "\n" . b
  where b :: Xml a -> [Builder]
        b (Element               n a c) = let subs = b c
                                          in if length subs <= 1
                                             then [mconcat [element n a, mconcat subs, "</", mconcat (b n), ">"]]
                                             else [element n a] ++ indent subs ++ [mconcat ["</", mconcat (b n), ">"]]
        b (Attribute             k v  ) = [attribute k v]
        b (Text                  t    ) = [fromLazyText (strip t)]
        b (CData                 d    ) = [fromLazyText d]
        b (Comment               c    ) = [comment c]
        b (ProcessingInstruction p v  ) = [processingIntruction p v]
        b (NodeSet               ns   ) = concatMap b (noSpace ns)
        b (AttributeList         as   ) = [attributeList as]
        b (QualifiedName         n    ) = [qualifiedName n]

noSpace :: [Xml Node] -> [Xml Node]
noSpace s = filter trim s
  where trim :: Xml Node -> Bool
        trim (Text xs) = strip xs /= ""
        trim _         = True

isFlat :: Xml [Node] -> Bool
isFlat (NodeSet s) = length (filter noText s) < 2
  where noText :: Xml Node -> Bool
        noText (Text {}) = False
        noText _         = True



element :: Xml Name -> Xml [Attr] -> Builder
element n a = mconcat ["<", compactBuilder n, compactBuilder a, ">"]

attribute :: Xml Name -> Text -> Builder
attribute k v = mconcat [compactBuilder k, "=\"", fromLazyText v, "\""]

comment :: Text -> Builder
comment c = mconcat ["<!-- ", fromLazyText c, " -->"]

processingIntruction :: Text -> Text -> Builder
processingIntruction p v = mconcat ["<?", fromLazyText p, " ", fromLazyText v, " ?>"]

attributeList :: [Xml Attr] -> Builder
attributeList as = mconcat (map (mappend " " . compactBuilder) as)

qualifiedName :: Text -> Builder
qualifiedName n = fromLazyText n

