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
, name
, text
, key
, value
, cdata
, instruction
, doctype

-- * Filter nodes.

, isCData
, isComment
, isDoctype
, isElem
, isProc
, isText

-- * Filter  elements.

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
, toProc

, noXml
, noAttributes
, emptyElem

-- * Creation with fixed components.

, mkElem
, mkAttr
, mkAttrValue
, mkText
, mkCData
, mkComment
, mkDoctype
, mkProc

-- * Converting to generic node.

, elemNode
, textNode
, cdataNode
, doctypeNode
, commentNode
, procNode

-- * Processing nodes.

, processChildren
, processAttributes
, modifyName
, processText

, processTopDownWhen
, processBottomUpWhen

-- * Parsing / printing.

, printXml
, parseXml

)
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Data.Text.Lazy (Text)
import Prelude hiding (elem, (.), id, concat)
import Xml.Tonic.Parse
import Xml.Tonic.Print

import qualified Xml.Tonic.Types as X

name :: Arrow (~>) => X.Element ~> Text
name = arr X.name

children :: ArrowList (~>) => X.Element ~> X.Node
children = arrL X.children

attributes :: ArrowList (~>) => X.Element ~> X.Attribute
attributes = arrL X.attributes

key :: Arrow (~>) => X.Attribute ~> Text
key = arr X.key

value :: Arrow (~>) => X.Attribute ~> Text
value = arr X.value

text :: ArrowList (~>) => X.Node ~> Text
text = arr X.text . isText

cdata :: ArrowList (~>) => X.Element ~> Text
cdata = arr X.cdata . isCData . children

instruction :: ArrowList (~>) => X.ProcessingInstruction ~> Text
instruction = arr X.instruction

doctype :: ArrowList (~>) => X.Doctype ~> Text
doctype = arr X.doctype

isElem :: ArrowList (~>) => X.Node ~> X.Element
isElem = arrL (\c -> case c of X.ElementNode e -> [e]; _ -> [])

isText :: ArrowList (~>) => X.Node ~> X.Text
isText = arrL (\c -> case c of X.TextNode t -> [t]; _ -> [])

isCData :: ArrowList (~>) => X.Node ~> X.CData
isCData = arrL (\c -> case c of X.CDataNode t -> [t]; _ -> [])

isComment :: ArrowList (~>) => X.Node ~> X.Comment
isComment = arrL (\c -> case c of X.CommentNode t -> [t]; _ -> [])

isDoctype :: ArrowList (~>) => X.Node ~> X.Doctype
isDoctype = arrL (\c -> case c of X.DoctypeNode t -> [t]; _ -> [])

isProc :: ArrowList (~>) => X.Node ~> X.ProcessingInstruction
isProc = arrL (\c -> case c of X.ProcessingInstructionNode p -> [p]; _ -> [])

elem :: (ArrowList (~>), ArrowChoice (~>)) => Text -> X.Element ~> X.Element
elem n = filterA (isA (==n) . name)

attr :: (ArrowChoice (~>), ArrowList (~>)) => Text -> X.Element ~> Text
attr n = value . filterA (isA (==n) . key) . attributes

child :: (ArrowList (~>), ArrowChoice (~>)) => Text -> X.Element ~> X.Element
child n = elem n . isElem . children

hasAttr :: (ArrowList (~>), ArrowChoice (~>)) => Text -> X.Element ~> X.Element
hasAttr n = filterA (isA (==n) . key . attributes)

deep :: (ArrowList (~>), ArrowPlus (~>)) => (X.Element ~> a) -> X.Element ~> a
deep e = e <+> deep e . isElem . children

deepWhen :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => X.Element ~> c -> X.Element ~> a -> X.Element ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . isElem . children

deepUnless :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => X.Element ~> c -> X.Element ~> a -> X.Element ~> a
deepUnless g = deepWhen (notA g)

toElem :: ArrowList (~>) => (a ~> Text) -> (a ~> X.Attribute) -> (a ~> X.Node) -> a ~> X.Element
toElem q as cs = proc i ->
  do n <- q -< i
     a <- list as -< i
     c <- list cs -< i
     id -< X.Element n a c

toAttr :: Arrow (~>) => (a ~> Text) -> (a ~> Text) -> a ~> X.Attribute
toAttr q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< X.Attribute n v

toText :: Arrow (~>) => Text ~> X.Text
toText = arr X.Text

toCData :: Arrow (~>) => Text ~> X.CData
toCData = arr X.CData

toComment :: Arrow (~>) => Text ~> X.Comment
toComment = arr X.Comment

toDoctype :: Arrow (~>) => Text ~> X.Doctype
toDoctype = arr X.Doctype

toProc :: Arrow (~>) => Text ~> X.ProcessingInstruction
toProc = arr X.ProcessingInstruction

noXml :: ArrowZero (~>) => a ~> X.Node
noXml = zeroArrow

noAttributes :: ArrowZero (~>) => a ~> X.Attribute
noAttributes = zeroArrow

emptyElem :: (ArrowList (~>), ArrowZero (~>)) => Text -> a ~> X.Element
emptyElem n = mkElem n noAttributes noXml

mkElem :: ArrowList (~>) => Text -> (a ~> X.Attribute) -> (a ~> X.Node) -> a ~> X.Element
mkElem n = toElem (arr (const n))

mkAttr :: Arrow (~>) => Text -> Text ~> X.Attribute
mkAttr k = toAttr (arr (const k)) id

mkAttrValue :: Arrow (~>) => Text -> Text -> a ~> X.Attribute
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow (~>) => Text -> a ~> X.Text
mkText t = toText . arr (const t)

mkCData :: Arrow (~>) => Text -> a ~> X.CData
mkCData t = toCData . arr (const t)

mkComment :: Arrow (~>) => Text -> a ~> X.Comment
mkComment t = toComment . arr (const t)

mkDoctype :: Arrow (~>) => Text -> a ~> X.Doctype
mkDoctype t = toDoctype . arr (const t)

mkProc :: Arrow (~>) => Text -> a ~> X.ProcessingInstruction
mkProc t = toProc . arr (const t)

elemNode :: Arrow (~>) => X.Element ~> X.Node
elemNode = arr X.ElementNode

textNode :: Arrow (~>) => X.Text ~> X.Node
textNode = arr X.TextNode

cdataNode :: Arrow (~>) => X.CData ~> X.Node
cdataNode = arr X.CDataNode

doctypeNode :: Arrow (~>) => X.Doctype ~> X.Node
doctypeNode = arr X.DoctypeNode

commentNode :: Arrow (~>) => X.Comment ~> X.Node
commentNode = arr X.CommentNode

procNode :: Arrow (~>) => X.ProcessingInstruction ~> X.Node
procNode = arr X.ProcessingInstructionNode

processChildren :: ArrowList (~>) => (X.Element ~> X.Node) -> X.Element ~> X.Element
processChildren p = toElem name attributes p 

processAttributes :: ArrowList (~>) => (X.Element ~> X.Attribute) -> X.Element ~> X.Element
processAttributes p = toElem name p children

modifyName :: ArrowList (~>) => (X.Element ~> Text) -> X.Element ~> X.Element
modifyName p = toElem p attributes children

processText :: ArrowList (~>) => (Text ~> Text) -> X.Element ~> X.Element
processText a = processChildren (textNode . toText . a . text . children)

processTopDownWhen :: (ArrowChoice (~>), ArrowList (~>)) => (X.Element ~> c) -> (X.Element ~> X.Element) -> X.Element ~> X.Element
processTopDownWhen c a = ifA c id (processChildren (elemNode . processTopDownWhen c a) . a)

processBottomUpWhen :: (ArrowChoice (~>), ArrowList (~>)) => (X.Element ~> c) -> (X.Element ~> X.Element) -> X.Element ~> X.Element
processBottomUpWhen c a = ifA c id (a . processChildren (elemNode . processBottomUpWhen c a))

printXml :: Arrow (~>) => X.Node ~> Text
printXml = arr (printer . return)

parseXml :: ArrowList (~>) => Text ~> X.Node
parseXml = arrL parser

