{-# LANGUAGE OverloadedStrings #-}
module Xml.Tonic
(

-- * Transforming XML using list arrows.

-- | These are the three most common actions when processing XML: either you
-- want to produce an XML document from some Haskell data structure, you want
-- to parse an existing XML document into a Haskell value, or you want to
-- transform an XML document. These three function let you process XML using a
-- simple list arrow.
 
  construct
, destruct
, transform

-- * List arrows for processing XML.
, module Xml.Tonic.Arrow

-- * XML tree datatypes.
, module Xml.Tonic.Types

-- * XML Parser and pretty printer.
, parser
, printer

)
where

import Xml.Tonic.Arrow
import Xml.Tonic.Types hiding
  ( attributes, text, cdata, doctype, value
  , children, name, key, instruction, textNode
  , cdataNode, doctypeNode, commentNode
  , processingInstructionNode
  )
import qualified Data.Text.Lazy as T
import qualified Xml.Tonic.Parse as Parse
import qualified Xml.Tonic.Print as Print
import Xml.Tonic.Transform

-- | Parse a lazy text into an XML tree. This function is just a renamed export
-- of 'Parse.xml'.

parser :: T.Text -> Xml
parser = Parse.xml

-- | Print an XML tree to a lazy text. This function is just a renamed export
-- of 'Print.xml'.

printer :: Xml -> T.Text
printer = Print.xml

