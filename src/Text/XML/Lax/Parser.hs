{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , GADTs
  #-}
module Text.XML.Lax.Parser where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Text (Text, isPrefixOf)
import Prelude hiding (until)
import Text.XML.Lax.Types
import qualified Data.Text as T


parse :: Text -> Xml [Node]
parse = runParser nodes

type Parser a = StateT Text (ReaderT [Text] (MaybeT Identity)) a

noParse :: a
noParse = error "This is a bug, please report."

runParser :: Parser a -> Text -> a
runParser parser input
  = runIdentity
  . fmap (fromMaybe noParse)
  . runMaybeT
  . flip runReaderT []
  . flip evalStateT input
  $ parser

progress :: Int -> Parser ()
progress n = modify (T.drop n)

token :: Text -> Parser Text
token tok =
  do st <- get
     if isPrefixOf tok st
       then tok <$ progress (T.length tok)
       else fail ""

until :: Text -> Parser Text
until tok =
  do st <- get
     case T.find tok st of
       []       -> fail ""
       (p, _):_ -> p <$ progress (T.length p + T.length tok)

while :: (Char -> Bool) -> Parser Text
while f =
  do st <- get
     case T.takeWhile f st of
       ""  -> fail ""
       tok -> tok <$ progress (T.length tok)

option :: Alternative f => a -> f a -> f a
option d p = p <|> pure d

qname :: Text -> QName
qname = QName ""

proc :: Parser (Xml Node)
proc = Proc "todo" <$> (token "<?" *> until "?>")

comment :: Parser (Xml Node)
comment = Cmnt <$> (token "<!--" *> until "-->")

node :: Parser (Xml Node)
node = comment <|> proc <|> element <|> text

nodes :: Parser (Xml [Node])
nodes = List <$> many node

element :: Parser (Xml Node)
element =
  do (tag, as) <- (,) <$> space open <*> space attributes
     local (tag:) (Elem (qname tag) as <$> space (self <|> rest tag))

  where
  open     = token "<" *> name
  name     = while (not . (`elem` " \r\n/>"))
  self     = List [] <$ token "/>"
  rest t   = token ">" *> nodes <* close t
  close t  = optional (token "</" *> token t <* token ">")

text :: Parser (Xml Node)
text = Text <$> while (/= '<')

-- Attribute parsing.

key :: Parser Text
key = while (not . (`elem` " \r\n=>/"))

value :: Parser Text
value = squoted <|> dquoted <|> unquoted
  where dquoted   = token "\"" *> until "\""
        squoted   = token "'" *> until "'"
        unquoted = while (not . (`elem` " \r\n>/"))

attribute :: Parser (Xml Attr)
attribute = Attr <$> (qname <$> key) <*> option "" (token "=" *> option "" value)

attributes :: Parser (Xml [Attr])
attributes = List <$> many (space attribute)

space :: Parser a -> Parser a
space p = p <* optional (while isSpace)

nospace :: Parser Text
nospace = while (not . isSpace)

