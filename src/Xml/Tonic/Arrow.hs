{-# LANGUAGE
    TypeOperators
  , ScopedTypeVariables
  , Arrows
  , GADTs
  #-}

{- | List arrows for querying, creating and modifying XML trees. -}
module Xml.Tonic.Arrow
(

-- * Selection.

  attributes
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
, deepUnless

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
, modifyName
, processText
, processDeep

-- * Parsing / printing.

-- , printXml
-- , parseXml

)
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Prelude hiding (elem, (.), id, concat)
import Xml.Tonic.Types hiding (name, key, value, attributes, children, text)
import qualified Xml.Tonic.Types as X
import qualified Data.Text.Lazy as T

-- import qualified Xml.Tonic.Parse as Parse
-- import qualified Xml.Tonic.Print as Print

name :: Arrow (~>) => Element ~> T.Text
name = arr X.name

children :: ArrowList (~>) => Element ~> Child
children = arrL X.children

attributes :: ArrowList (~>) => Element ~> Attribute
attributes = arrL X.attributes

key :: Arrow (~>) => Attribute ~> T.Text
key = arr X.key

value :: Arrow (~>) => Attribute ~> T.Text
value = arr X.value

text :: ArrowList (~>) => Element ~> T.Text
text = arr X.text . isText . children

isElem :: ArrowList (~>) => Child ~> Element
isElem = arrL (\c -> case c of ElementChild e -> [e]; _ -> [])

isText :: ArrowList (~>) => Child ~> Text
isText = arrL (\c -> case c of TextChild t -> [t]; _ -> [])

isCData :: ArrowList (~>) => Child ~> CData
isCData = arrL (\c -> case c of CDataChild t -> [t]; _ -> [])

isComment :: ArrowList (~>) => Child ~> Comment
isComment = arrL (\c -> case c of CommentChild t -> [t]; _ -> [])

isDoctype :: ArrowList (~>) => Child ~> Doctype
isDoctype = arrL (\c -> case c of DoctypeChild t -> [t]; _ -> [])

isProcessingInstruction :: ArrowList (~>) => Child ~> ProcessingInstruction
isProcessingInstruction = arrL (\c -> case c of ProcessingInstructionChild p -> [p]; _ -> [])

elem :: (ArrowList (~>), ArrowChoice (~>)) => T.Text -> Element ~> Element
elem n = filterA (isA (==n) . name)

attr :: (ArrowChoice (~>), ArrowList (~>)) => T.Text -> Element ~> T.Text
attr n = key . filterA (isA (==n) . value) . attributes

child :: (ArrowList (~>), ArrowChoice (~>)) => T.Text -> Element ~> Element
child n = elem n . isElem . children

hasAttr :: (ArrowList (~>), ArrowChoice (~>)) => T.Text -> Element ~> Element
hasAttr n = filterA (isA (==n) . key . attributes)

deep :: (ArrowList a, ArrowPlus a) => a Element c -> a Element c
deep e = e <+> deep e . isElem . children

deepWhen :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Element ~> c -> Element ~> a -> Element ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . isElem . children

deepUnless :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Element ~> c -> Element ~> a -> Element ~> a
deepUnless g = deepWhen (notA g)

toElem :: (ArrowPlus (~>), ArrowList (~>)) => (a ~> T.Text) -> [a ~> Attribute] -> [a ~> Child] -> a ~> Child
toElem q as cs = proc i ->
  do n <- q -< i
     a <- list (concatA as) -< i
     c <- list (concatA cs) -< i
     id -< ElementChild (Element n a c)

toAttr :: (Arrow (~>)) => (a ~> T.Text) -> (a ~> T.Text) -> a ~> Attribute
toAttr q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< Attribute n v

toText :: Arrow (~>) => T.Text ~> Child
toText = arr (TextChild . Text)

toCData :: Arrow (~>) => T.Text ~> Child
toCData = arr (CDataChild . CData)

toComment :: Arrow (~>) => T.Text ~> Child
toComment = arr (CommentChild . Comment)

toDoctype :: Arrow (~>) => T.Text ~> Child
toDoctype = arr (DoctypeChild . Doctype)

toProcessingInstruction :: Arrow (~>) => T.Text ~> Child
toProcessingInstruction = arr (ProcessingInstructionChild . ProcessingInstruction)

mkElem :: (ArrowPlus (~>), ArrowList (~>))
        => T.Text -> [a ~> Attribute] -> [a ~> Child] -> a ~> Child
mkElem n = toElem (arr (const n))

mkAttr :: Arrow (~>) => T.Text -> T.Text ~> Attribute
mkAttr k = toAttr (arr (const k)) id

mkAttrValue :: Arrow (~>) => T.Text -> T.Text -> a ~> Attribute
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow (~>) => T.Text -> a ~> Child
mkText t = toText . arr (const t)

mkCData :: Arrow (~>) => T.Text -> a ~> Child
mkCData t = toCData . arr (const t)

mkComment :: Arrow (~>) => T.Text -> a ~> Child
mkComment t = toComment . arr (const t)

mkDoctype :: Arrow (~>) => T.Text -> a ~> Child
mkDoctype t = toDoctype . arr (const t)

mkProcessingInstruction :: Arrow (~>) => T.Text -> a ~> Child
mkProcessingInstruction t = toProcessingInstruction . arr (const t)

processChildren :: (ArrowPlus (~>), ArrowList (~>)) => (Element ~> Child) -> Element ~> Child
processChildren p = toElem name [attributes] [p] 

processAttributes :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => (Element ~> Attribute) -> Element ~> Child
processAttributes p = toElem name [p] [children]

modifyName :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => (Element ~> T.Text) -> Element ~> Child
modifyName p = toElem p [attributes] [children]

processText :: ArrowList (~>) => T.Text ~> T.Text -> Element ~> Child
processText a = toText . a . text

processDeep :: (ArrowList (~>), ArrowChoice (~>), ArrowPlus (~>)) => (Element ~> c) -> (Element ~> Child) -> Element ~> Child
processDeep c a = ifA c a (processChildren (processDeep c a))

-- printXml :: Arrow (~>) => Element ~> T.Text
-- printXml = arr Print.pretty

-- parseXml :: ArrowList (~>) => T.Text ~> Element
-- parseXml = nodes . arr Parse.xml

