{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , GADTs
  #-}
module Xml.Tonic.Parse
(
-- * Top level functions for parsing XML or HTML.
  xml
, html

-- * Individual parser functions.

, nodeset
, node
, element
, attribute
, attributeList
, text
, processingInstruction
, comment
, cdata

-- * The parser monad.
, Parser
, runParser

-- * Primitive parsers.
, token
, until
, while

)
where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Text.Lazy (Text, isPrefixOf)
import Prelude hiding (until)
import Xml.Tonic.Types
import qualified Data.Text.Lazy as T

xml :: Text -> Xml [Node]
xml = runParser nodeset

html :: Text -> Xml [Node]
html = runParser nodeset

processingInstruction :: Parser (Xml Node)
processingInstruction = ProcessingInstruction <$> (token "<?" *> until "?>")

comment :: Parser (Xml Node)
comment = Comment <$> (token "<!--" *> until "-->")

node :: Parser (Xml Node)
node = comment <|> doctype <|> processingInstruction <|> element <|> text

nodeset :: Parser (Xml [Node])
nodeset = NodeSet <$> many node

element :: Parser (Xml Node)
element =
  do (t, as) <- (,) <$> free open <*> free attributeList
     local (t:) (Element (QualifiedName t) as <$> free (self <|> rest t))

  where
  open     = token "<" *> tag
  tag      = while (not . (`elem` " \r\n/>"))
  self     = NodeSet [] <$ token "/>"
  rest t   = token ">" *> nodeset <* close t
  close t  = optional (token "</" *> token t <* token ">")

text :: Parser (Xml Node)
text = Text <$> while (/= '<')

cdata :: Parser (Xml Node)
cdata = Text <$> while (/= '<')

-- Attribute parsing.

key :: Parser Text
key = while (not . (`elem` " \r\n=>/"))

value :: Parser Text
value = squoted <|> dquoted <|> unquoted
  where dquoted  = token "\"" *> until "\""
        squoted  = token "'" *> until "'"
        unquoted = while (not . (`elem` " \r\n>/"))
doctype :: Parser (Xml Node)
doctype = Doctype <$> (token "<!" *> until ">")

attribute :: Parser (Xml Attr)
attribute = Attribute <$> (QualifiedName <$> key) <*> option "" (token "=" *> option "" value)

attributeList :: Parser (Xml [Attr])
attributeList = AttributeList <$> many (free attribute)

option :: Alternative f => a -> f a -> f a
option d p = p <|> pure d

free :: Parser a -> Parser a
free p = p <* optional (while isSpace)

newtype Parser a = P { runP :: StateT Text (ReaderT [Text] (MaybeT Identity)) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadState  Text
           , MonadReader [Text]
           )

noParse :: a
noParse = error "This is a bug, please report."

runParser :: Parser a -> Text -> a
runParser parser input
  = runIdentity
  . fmap (fromMaybe noParse)
  . runMaybeT
  . flip runReaderT []
  . flip evalStateT input
  . runP
  $ parser

token :: Text -> Parser Text
token tok =
  do v <- gets (isPrefixOf tok)
     if v then tok <$ modify (T.drop (T.length tok))
          else fail []

until :: Text -> Parser Text
until tok =
  do v <- gets (T.find tok)
     case v of
       []       -> fail []
       (p, _):_ -> p <$ modify (T.drop (T.length p + T.length tok))

while :: (Char -> Bool) -> Parser Text
while f =
  do v <- gets (T.takeWhile f)
     case v of
       ""  -> fail []
       tok -> tok <$ modify (T.drop (T.length tok))

