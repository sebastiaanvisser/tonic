{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  #-}
module Parser
(
-- * Parser monad and transformer.
  ParserT (..)
, runParserT

, Parser
, runParser

-- Parser status types.
, Position
, ParseError (..)

-- * Primitive parsers.
, token
, delim
, satisfy

-- * Derived parsers.
, option
, sep1
, sep
)
where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Errors
import Control.Monad.Identity
import Control.Monad.State
import Data.String
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as T

import Debug.Trace

type Position = Integer

data ParseError
  = Token   Text Position
  | Delim   Text Position
  | Satisfy Position
  | NonProductive
  deriving (Eq, Ord, Show)

type ParseState = (Position, Text)

newtype ParserT m a = Parser { runParser_ :: StateT ParseState (ErrorsT [ParseError] m) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadState ParseState
           , MonadError [ParseError]
           )

type Parser a = ParserT Identity a

instance (Functor m, Monad m) => IsString (ParserT m Text) where
  fromString = token . fromString

runParserT :: (Functor m, Monad m) => ParserT m a -> Text -> m (Either [ParseError] a)
runParserT parser input
  = runErrorsT
  . flip evalStateT (0, input)
  . runParser_
  $ parser

runParser :: Parser a -> Text -> Either [ParseError] a
runParser p = runIdentity . runParserT p

token :: (Functor m, Monad m) => Text -> ParserT m Text
token tok =
  do (pos, st) <- get
     if isPrefixOf tok st
       then tok <$ shift (T.length tok)
       else throwError [Token tok pos]

delim :: (Functor m, Monad m) => Text -> ParserT m Text
delim tok =
  do (pos, st) <- get
     case T.find tok st of
       []       -> throwError [Delim tok pos]
       (p, _):_ -> p <$ shift (T.length p + T.length tok)

satisfy :: (Functor m, Monad m) => (Char -> Bool) -> ParserT m Text
satisfy f =
  do (pos, st) <- get
     case T.takeWhile f st of
       ""  -> throwError [Satisfy pos]
       tok -> tok <$ shift (T.length tok)

shift :: Monad m => Int -> ParserT m ()
shift n = modify (\(pos, st) -> (pos + fromIntegral n, T.drop n st))

-------------------------------------

option :: Alternative f => a -> f a -> f a
option d p = p <|> pure d

sep1 :: Alternative f => f sep -> f a -> f [a]
sep1 s p = (:) <$> p <*> many (s *> p)

sep :: Alternative f => f sep -> f a -> f [a]
sep s p = option [] (sep1 s p)

