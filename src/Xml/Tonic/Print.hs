{-# LANGUAGE OverloadedStrings, GADTs #-}
module Xml.Tonic.Print where

import Data.Monoid
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy  as T
import qualified Xml.Tonic.Types as X

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

xml :: [X.Child] -> T.Text
xml = toLazyText . mconcat . children

element :: X.Element -> [Builder]
element (X.Element n a c) =
  let subs   = children c
      attrs  = mconcat (attributes a)
      open s = "<" <> fromLazyText n <> attrs
                   <> if s then "/>" else ">"
      close  = "</" <> fromLazyText n <> ">"
  in case subs of
    [] -> open True  : subs
    _  -> open False : subs ++ [close]

children :: [X.Child] -> [Builder]
children = concatMap child

child :: X.Child -> [Builder]
child (X.ElementChild               t) = element               t
child (X.TextChild                  t) = text                  t
child (X.CDataChild                 t) = cdata                 t
child (X.CommentChild               t) = comment               t
child (X.DoctypeChild               t) = doctype               t
child (X.ProcessingInstructionChild t) = processingInstruction t

attributes :: [X.Attribute] -> [Builder]
attributes = concatMap attribute

attribute :: X.Attribute -> [Builder]
attribute (X.Attribute k v) = [fromLazyText k <> "=\"" <> fromLazyText v <> "\""]

text :: X.Text -> [Builder]
text (X.Text t) = [fromLazyText t]

cdata :: X.CData -> [Builder]
cdata (X.CData d) = ["<![CDATA[" <> fromLazyText d <> "]]>"]

comment :: X.Comment -> [Builder]
comment (X.Comment c) = ["<!-- " <> fromLazyText c <> " -->"]

doctype :: X.Doctype -> [Builder]
doctype (X.Doctype d) = ["<!" <> fromLazyText d <> " >"]

processingInstruction :: X.ProcessingInstruction -> [Builder]
processingInstruction (X.ProcessingInstruction i) = ["<?" <> fromLazyText i <> " ?>"]

