{-# LANGUAGE
    TypeOperators
  , ScopedTypeVariables
  , Arrows
  , GADTs
  #-}

{- |
List arrows for querying, creating and modifying XML trees.
-}
module Xml.Tonic.Arrow
(

-- * Selection.

  nodes
, attributes
, children
, key
, name
, text
, value

-- * Filter.

, isCData
, isComment
, isDoctype
, isElem
, isProcessingInstruction
, isText

-- * By name.

, elem
, attr
, child

, hasAttr

-- * Deep selection.

, deep
, deepWhen
, deepWhenNot
, deepText

-- * Creation with only arrow components.

, toElem
, toAttr
, toText
, toCData
, toComment
, toDoctype
, toProcessingInstruction

-- * Creation with fixed components.

, mkElem
, mkAttr
, mkAttrValue
, mkText
, mkCData
, mkComment
, mkDoctype
, mkProcessingInstruction

-- * Processing child nodes.

, processChildren
, processAttributes
, processText
, processDeep

-- * Parsing / printing.

, printXml
, parseXml

)
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Data.Text.Lazy
import Prelude hiding (elem, (.), id, concat)
import Xml.Tonic.Types hiding (name, key, value, attributes, children, text)
import qualified Xml.Tonic.Types as X
import qualified Xml.Tonic.Parse as Parse
import qualified Xml.Tonic.Print as Print

nodes :: ArrowList (~>) => Xml [Node] ~> Xml Node
nodes = unlistA . arr nodeSet

name :: ArrowList (~>) => Xml Node ~> Text
name = arr (qname . X.name) . isElem

children :: ArrowList (~>) => Xml Node ~> Xml Node
children = arrL (nodeSet . X.children) . isElem

attributes :: ArrowList (~>) => Xml Node ~> Xml Attr
attributes = arrL (attributeList . X.attributes) . isElem

key :: Arrow (~>) => Xml Attr ~> Text
key = arr (qname . X.key)

value :: Arrow (~>) => Xml Attr ~> Text
value = arr X.value

text :: ArrowList (~>) => Xml Node ~> Text
text = arr X.text . isText

isElem, isText, isCData, isComment, isDoctype, isProcessingInstruction
  :: ArrowList (~>) => Xml Node ~> Xml Node

isElem                  = isA (\c -> case c of Element               {} -> True; _ -> False)
isText                  = isA (\c -> case c of Text                  {} -> True; _ -> False)
isCData                 = isA (\c -> case c of CData                 {} -> True; _ -> False)
isComment               = isA (\c -> case c of Comment               {} -> True; _ -> False)
isDoctype               = isA (\c -> case c of Doctype               {} -> True; _ -> False)
isProcessingInstruction = isA (\c -> case c of ProcessingInstruction {} -> True; _ -> False)

elem :: (ArrowList (~>), ArrowChoice (~>)) => Text -> Xml Node ~> Xml Node
elem n = filterA (isA (==n) . name . isElem)

attr :: (ArrowList (~>), ArrowChoice (~>)) => Text -> Xml Node ~> Text
attr n = key . filterA (isA (==n) . value) . attributes

child :: (ArrowList (~>), ArrowChoice (~>)) => Text -> Xml Node ~> Xml Node
child n = elem n . children

hasAttr :: (ArrowList (~>), ArrowChoice (~>)) => Text -> Xml Node ~> Xml Node
hasAttr n = filterA (isA (==n) . key . attributes)

deep :: (ArrowList (~>), ArrowPlus (~>)) => (Xml Node ~> a) -> (Xml Node ~> a)
deep e = e <+> deep e . children

deepWhen :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Xml Node ~> c -> Xml Node ~> a -> Xml Node ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepWhenNot :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Xml Node ~> c -> Xml Node ~> a -> Xml Node ~> a
deepWhenNot g = deepWhen (notA g)

deepText :: (ArrowPlus (~>), ArrowList (~>)) => Xml Node ~> Text
deepText = arr concat . collect (deep text)

toElem :: (ArrowPlus (~>), ArrowList (~>)) => (a ~> Text) -> [a ~> Xml Attr] -> [a ~> Xml Node] -> a ~> Xml Node
toElem q as cs = proc i ->
  do n <- q -< i
     a <- collect (concatA as) -< i
     c <- collect (concatA cs) -< i
     id -< Element (QualifiedName n) (AttributeList a) (NodeSet c)

toAttr :: Arrow (~>) => (a ~> Text) -> (a ~> Text) -> a ~> Xml Attr
toAttr q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< Attribute (QualifiedName n) v

toText :: Arrow (~>) => Text ~> Xml Node
toText = arr Text

toCData :: Arrow (~>) => Text ~> Xml Node
toCData = arr CData

toComment :: Arrow (~>) => Text ~> Xml Node
toComment = arr Comment

toDoctype :: Arrow (~>) => Text ~> Xml Node
toDoctype = arr Doctype

toProcessingInstruction :: Arrow (~>) => Text ~> Xml Node
toProcessingInstruction = arr ProcessingInstruction

mkElem :: (ArrowPlus (~>), ArrowList (~>))
        => Text -> [a ~> Xml Attr] -> [a ~> Xml Node] -> a ~> Xml Node
mkElem n = toElem (arr (const n))

mkAttr :: Arrow (~>) => Text -> Text ~> Xml Attr
mkAttr k = toAttr (arr (const k)) id

mkAttrValue :: Arrow (~>) => Text -> Text -> a ~> Xml Attr
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow (~>) => Text -> a ~> Xml Node
mkText t = toText . arr (const t)

mkCData :: Arrow (~>) => Text -> a ~> Xml Node
mkCData t = toCData . arr (const t)

mkComment :: Arrow (~>) => Text -> a ~> Xml Node
mkComment t = toComment . arr (const t)

mkDoctype :: Arrow (~>) => Text -> a ~> Xml Node
mkDoctype t = toDoctype . arr (const t)

mkProcessingInstruction :: Arrow (~>) => Text -> a ~> Xml Node
mkProcessingInstruction t = toProcessingInstruction . arr (const t)

processChildren :: (ArrowList (~>), ArrowChoice (~>), ArrowPlus (~>)) => (Xml Node ~> Xml Node) -> Xml Node ~> Xml Node
processChildren p = toElem name [attributes] [p . children] `when` isElem

processAttributes :: (ArrowList (~>), ArrowChoice (~>), ArrowPlus (~>)) => (Xml Attr ~> Xml Attr) -> Xml Node ~> Xml Node
processAttributes p = toElem name [p . attributes] [children] `when` isElem

processText :: ArrowList (~>) => Text ~> Text -> Xml Node ~> Xml Node
processText a = toText . a . text

processDeep :: (ArrowList (~>), ArrowChoice (~>), ArrowPlus (~>)) => Xml Node ~> c -> Xml Node ~> Xml Node -> Xml Node ~> Xml Node
processDeep c a = ifA c a (processChildren (processDeep c a))

printXml :: Arrow (~>) => Xml Node ~> Text
printXml = arr Print.pretty

parseXml :: ArrowList (~>) => Text ~> Xml Node
parseXml = nodes . arr Parse.xml

