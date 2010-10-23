{-# LANGUAGE OverloadedStrings, GADTs #-}
module Xml.Tonic.Print
(
-- * Top level pretty printers.

  pretty
, compact
, asIs

-- * Configurable text builder.

, Indent
, Compact
, Closing
, Trim
, Filter

, builder

)
where

import Data.List
import Data.Monoid
import Data.Text.Lazy (Text, strip)
import Data.Text.Lazy.Builder
import Xml.Tonic.Types

{- |
  - Indent with two spaces per level.

  - Every item on its own line.

  - Self close allowed for all tags.

  - Strip leading whitespace from text nodes.

  - Filter all whitespace only text nodes.
-}

pretty :: Xml a -> Text
pretty = toLazyText
       . builder
         (map (mappend "  "))
         (mconcat . intersperse "\n")
         (const True)
         strip
         noSpace

asIs :: Xml a -> Text
asIs = toLazyText . builder id mconcat (const False) id id

compact :: Xml a -> Text
compact = toLazyText . builder id mconcat (const True) strip noSpace

type Indent  = [Builder] -> [Builder]
type Compact = [Builder] -> Builder
type Closing = Xml Name -> Bool
type Trim    = Text -> Text
type Filter  = [Xml Node] -> [Xml Node]

builder :: Indent -> Compact -> Closing -> Trim -> Filter -> Xml a -> Builder
builder indent join closing trim ft = join . b
  where b :: Xml a -> [Builder]
        b (Element               n a c) = let subs = b c
                                              tagOpen s = mconcat ["<", mconcat (b n), mconcat (b a), if s then "/>" else ">"]
                                          in case (length subs, closing n) of
                                               (0, True) -> [tagOpen True]
                                               (1, _)    -> [mconcat [tagOpen False, mconcat subs, "</", mconcat (b n), ">"]]
                                               _         -> tagOpen False : indent subs ++ [mconcat ["</", mconcat (b n), ">"]]
        b (Attribute             k v  ) = [mconcat [mconcat (b k), "=\"", fromLazyText v, "\""]]
        b (Text                  t    ) = [fromLazyText (trim t)]
        b (CData                 d    ) = [fromLazyText d]
        b (Comment               c    ) = [mconcat ["<!-- ", fromLazyText c, " -->"]]
        b (ProcessingInstruction p    ) = [mconcat ["<?", fromLazyText p, " ?>"]]
        b (Doctype               d    ) = [mconcat ["<!", fromLazyText d, " >"]]
        b (NodeSet               ns   ) = concatMap b (ft ns)
        b (AttributeList         as   ) = [mconcat (map (mappend " " . mconcat . b) as)]
        b (QualifiedName         n    ) = [fromLazyText n]

noSpace :: [Xml Node] -> [Xml Node]
noSpace s = filter trim s
  where trim :: Xml Node -> Bool
        trim (Text xs) = strip xs /= ""
        trim _         = True


