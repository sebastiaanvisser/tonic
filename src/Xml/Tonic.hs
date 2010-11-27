module Xml.Tonic
(

-- * XML Parser and pretty printer.
  parser
, printer

-- * XML tree datatypes.
, module Xml.Tonic.Types

-- * Arrows for processing XML.
, module Xml.Tonic.Arrow
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

-- | Parse a lazy text into an XML tree. This function is just a renamed export
-- of 'Parse.xml'.

parser :: T.Text -> Xml
parser = Parse.xml

-- | Print an XML tree to a lazy text. This function is just a renamed export
-- of 'Print.xml'.

printer :: Xml -> T.Text
printer = Print.xml

