{-# LANGUAGE OverloadedStrings #-}
module XmlParser where

import Control.Applicative
import Data.Char
import Data.Text (Text)
import Parser
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Xml

import Debug.Trace







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
  do tag <- open
     uncurry (Elem (qname tag)) <$> rest tag
  where
    rest t   = self <|> (,) <$> attrs <*> nodes <* close t
    self     = (List [], List []) <$ (space *> token "/>")
    open     = token "<" *> name
    name     = satisfy (not . (`elem` " \r\n>"))
    attrs    = option (List []) (space *> attributes) <* token ">"
    close t  = token "</" *> token t <* token ">"

text :: Parser (Xml Node)
text = Text <$> satisfy (/= '<')

-- Attribute parsing.

key :: Parser Text
key = satisfy (not . (`elem` " \r\n=<>"))

value :: Parser Text
value = quoted <|> unquoted
  where quoted   = token "\"" *> delim "\""
        unquoted = satisfy (not . (`elem` " \r\n>"))

attribute :: Parser (Xml Attr)
attribute = Attr <$> (qname <$> key) <*> option "" (token "=" *> option "" value)

attributes :: Parser (Xml [Attr])
attributes = List <$> sep1 space attribute



space :: Parser Text
space = option "" (satisfy isSpace)

nospace :: (Functor m, Monad m) => ParserT m Text
nospace = satisfy (not . isSpace)

nospaceOrClose :: Parser Text
nospaceOrClose = satisfy (\x -> x /= '>' && not (isSpace x))


main :: IO ()
main =
  do html <- T.readFile "test.html"
     case runParser node html of
       Left  e -> print e
       Right r -> print r


-- Todo: test everything for opening and never closing!

