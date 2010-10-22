{-# LANGUAGE OverloadedStrings, GADTs #-}
module XmlParser where

import Control.Applicative
import Data.Char
import Parser
import Data.Text (Text)
import qualified Data.Text.IO as T

import Xml




qname :: Text -> QName
qname = QName ""

proc :: Parser (Xml Node)
proc = Proc "todo" <$> (token "<?" *> delim "?>")

comment :: Parser (Xml Node)
comment = Cmnt <$> (token "<!--" *> delim "-->")

node :: Parser (Xml Node)
node = comment <|> proc <|> element <|> text

nodes :: Parser (Xml [Node])
nodes = List <$> many node

element :: Parser (Xml Node)
element =
  do (tag, as) <- (,) <$> space open <*> space attributes
     Elem (qname tag) as <$> space (self <|> rest tag)

  where
  open     = token "<" *> name
  name     = satisfy (not . (`elem` " \r\n/>"))
  self     = List [] <$ token "/>"
  rest t   = token ">" *> nodes <* close t
  close t  = token "</" *> token t <* token ">"

text :: Parser (Xml Node)
text = Text <$> satisfy (/= '<')

-- Attribute parsing.

key :: Parser Text
key = satisfy (not . (`elem` " \r\n=>/"))

value :: Parser Text
value = squoted <|> dquoted <|> unquoted
  where dquoted   = token "\"" *> delim "\""
        squoted   = token "'" *> delim "'"
        unquoted = satisfy (not . (`elem` " \r\n>/"))

attribute :: Parser (Xml Attr)
attribute = Attr <$> (qname <$> key) <*> option "" (token "=" *> option "" value)

attributes :: Parser (Xml [Attr])
attributes = List <$> many (space attribute)



space :: Parser a -> Parser a
space p = p <* optional (satisfy isSpace)

nospace :: (Functor m, Monad m) => ParserT m Text
nospace = satisfy (not . isSpace)

nospaceOrClose :: Parser Text
nospaceOrClose = satisfy (\x -> x /= '>' && not (isSpace x))

main :: IO ()
main =
  do html <- T.readFile "test.html"
     case runParser nodes html of
       Left  e -> print e
       Right r -> print r

-- Todo: test everything for opening and never closing!

