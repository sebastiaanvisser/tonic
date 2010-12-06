{-# LANGUAGE OverloadedStrings, GADTs #-}
module Xml.Tonic.Print
(

-- * Top level XML printers.
  xml
-- , compact

-- * As-is text builders.
, element
, nodes
, node
, attributes
, attribute
, text
, cdata
, comment
, doctype
, processingInstruction

-- Compact text builders.
-- , elementC
-- , nodesC
-- , nodeC
-- , textC
)
where

import Data.Monoid
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy  as T
import qualified Xml.Tonic.Types as X

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- | Print an XML tree to a lazy text.

-- (parse . print == id) is guaranteed, (parse . print == id) is /not/ guaranteed.

xml :: X.Xml -> T.Text
xml = toLazyText . mconcat . nodes

-- | Print an XML tree to a lazy text in a compacted form, stripped from redundant whitespace.

-- Both (parse . print == id) and (parse . print == id) are /not/ guaranteed.

-- compact :: X.Xml -> T.Text
-- compact = toLazyText . mconcat . nodesC

-- As-is builders.

nodes :: [X.Node] -> [Builder]
nodes = concatMap node

node :: X.Node -> [Builder]
node (X.ElementNode               t) = element               t
node (X.TextNode                  t) = text                  t
node (X.CDataNode                 t) = cdata                 t
node (X.CommentNode               t) = comment               t
node (X.DoctypeNode               t) = doctype               t
node (X.ProcessingInstructionNode t) = processingInstruction t

element :: X.Element -> [Builder]
element (X.Element n a c) =
  let subs   = nodes c
      attrs  = mconcat (attributes a)
      open s = "<" <> fromLazyText n <> attrs
                   <> if s then "/>" else ">"
      close  = "</" <> fromLazyText n <> ">"
  in case subs of
    [] -> open True  : subs
    _  -> open False : subs ++ [close]

attributes :: [X.Attribute] -> [Builder]
attributes = concatMap (mappend [" "] . attribute)

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

{-

-- Compact builders.

nodesC :: [X.Node] -> [Builder]
nodesC = concatMap nodeC

nodeC :: X.Node -> [Builder]
nodeC (X.ElementNode               t) = elementC              t
nodeC (X.TextNode                  t) = textC                 t
nodeC (X.CDataNode                 t) = cdata                 t
nodeC (X.CommentNode               t) = comment               t
nodeC (X.DoctypeNode               t) = doctype               t
nodeC (X.ProcessingInstructionNode t) = processingInstruction t

elementC :: X.Element -> [Builder]
elementC (X.Element n a c) =
  let subs   = nodesC c
      attrs  = mconcat (attributes a)
      open s = "<" <> fromLazyText n <> attrs
                   <> if s then "/>" else ">"
      close  = "</" <> fromLazyText n <> ">"
  in case subs of
    [] -> open True  : subs
    _  -> open False : subs ++ [close]

textC :: X.Text -> [Builder]
textC (X.Text t) = if T.null s then [] else [fromLazyText s]
  where s = T.replace "\n" " " (T.strip t)

-}

