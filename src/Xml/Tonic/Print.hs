{-# LANGUAGE OverloadedStrings, GADTs #-}
module Xml.Tonic.Print
(
-- * Top level pretty printers.
  pretty
, compact
, asis

-- * Builder.
, Config (..)
, builder

)
where

import Data.List
import Data.Monoid
import Data.Text.Lazy (Text, strip)
import Data.Text.Lazy.Builder
import Xml.Tonic.Types

pretty :: Xml a -> Text
pretty = toLazyText . builder (Config (map (mappend "  ")) (mconcat . intersperse "\n") (const True) strip noSpace True)

compact :: Xml a -> Text
compact = toLazyText . builder (Config id mconcat (const True) strip noSpace False)

asis :: Xml a -> Text
asis = toLazyText . builder (Config id mconcat (const True) id id False)

data Config = Config
  { cIndent  :: [Builder] -> [Builder]
  , cCompact :: [Builder] -> Builder
  , cClosing :: Xml Name -> Bool
  , cTrim    :: Text -> Text
  , cFilter  :: [Xml Node] -> [Xml Node]
  , cFlatten :: Bool
  }

builder :: Config -> Xml a -> Builder
builder (Config indent join closing trim skip flatten) = join . b
  where b :: Xml a -> [Builder]
        b (Element               n a c) = let subs   = b c
                                              open s = "<" <> mconcat (b n) <> mconcat (b a) <> if s then "/>" else ">"
                                              close  = "</" <> mconcat (b n) <> ">"
                                          in case (flatten, subs, closing n) of
                                            (False, [],  _) ->           open True  : indent  subs
                                            (False, _,   _) ->           open False : indent  subs ++ [close]
                                            (True,  [],  _) -> [mconcat [open True,   mconcat subs]]
                                            (True,  [_], _) -> [mconcat [open False,  mconcat subs,    close]]
                                            (True,  _,   _) ->           open False : indent  subs ++ [close]
        b (Attribute             k v  ) = [mconcat (b k) <> "=\"" <> fromLazyText v <> "\""]
        b (Text                  t    ) = [fromLazyText (trim t)]
        b (CData                 d    ) = ["<![CDATA[" <> fromLazyText d <> "]]>"]
        b (Comment               c    ) = ["<!-- " <> fromLazyText c <> "-->"]
        b (ProcessingInstruction p    ) = ["<?" <> fromLazyText p <> " ?>"]
        b (Doctype               d    ) = ["<!" <> fromLazyText d <> " >"]
        b (NodeSet               ns   ) = foldr (++) [] (map b (skip ns))
        b (AttributeList         as   ) = concatMap (\a -> " " : b a) as
        b (QualifiedName         n    ) = [fromLazyText n]

        (<>) = mappend

noSpace :: [Xml Node] -> [Xml Node]
noSpace s = filter trim s
  where trim :: Xml Node -> Bool
        trim (Text xs) = strip xs /= ""
        trim _         = True

