{-# LANGUAGE OverloadedStrings, GADTs #-}
module XmlParser where

import Control.Applicative
import Control.Monad.Reader
import Data.Char
import Data.Text (Text)
import Parser
import Prelude hiding (until)
import Xml

qname :: Text -> QName
qname = QName ""

proc :: P (Xml Node)
proc = Proc "todo" <$> (token "<?" *> until "?>")

comment :: P (Xml Node)
comment = Cmnt <$> (token "<!--" *> until "-->")

node :: P (Xml Node)
node = comment <|> proc <|> element <|> text

nodes :: P (Xml [Node])
nodes = List <$> many node

element :: P (Xml Node)
element =
  do (tag, as) <- (,) <$> space open <*> space attributes
     local (tag:) (Elem (qname tag) as <$> space (self <|> rest tag))

  where
  open     = token "<" *> name
  name     = while (not . (`elem` " \r\n/>"))
  self     = List [] <$ token "/>"
  rest t   = token ">" *> nodes <* close t
  close t  = optional (token "</" *> token t <* token ">")

text :: P (Xml Node)
text = Text <$> while (/= '<')

-- Attribute parsing.

key :: P Text
key = while (not . (`elem` " \r\n=>/"))

value :: P Text
value = squoted <|> dquoted <|> unquoted
  where dquoted   = token "\"" *> until "\""
        squoted   = token "'" *> until "'"
        unquoted = while (not . (`elem` " \r\n>/"))

attribute :: P (Xml Attr)
attribute = Attr <$> (qname <$> key) <*> option "" (token "=" *> option "" value)

attributes :: P (Xml [Attr])
attributes = List <$> many (space attribute)

space :: P a -> P a
space p = p <* optional (while isSpace)

nospace :: P Text
nospace = while (not . isSpace)

nospaceOrClose :: P Text
nospaceOrClose = while (\x -> x /= '>' && not (isSpace x))

