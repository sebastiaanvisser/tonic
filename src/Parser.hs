{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  #-}
module Parser where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Errors
import Control.Monad.Identity
import Control.Monad.State
import Data.String
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as T

type Position = Integer

data ParseError
  = Token   Text Position
  | Delim   Text Position
  | Satisfy Position
  deriving (Eq, Ord, Show)

type ParseState = (Position, Text)

newtype Parser a = Parser { runParser_ :: StateT ParseState (ErrorsT [ParseError] Identity) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadState ParseState
           , MonadError [ParseError]
           )

runParser :: Parser a -> Text -> Either [ParseError] (a, (Position, Text))
runParser parser input
  = runIdentity
  . runErrorsT
  . flip runStateT (0, input)
  . runParser_
  $ parser

shift :: Int -> Parser ()
shift n = modify (\(pos, st) -> (pos + fromIntegral n, T.drop n st))

token :: Text -> Parser Text
token tok =
  do (pos, st) <- get
     if isPrefixOf tok st
       then tok <$ shift (T.length tok)
       else throwError [Token tok pos]

instance IsString (Parser Text) where
  fromString = token . fromString

delim :: Text -> Parser Text
delim tok =
  do (pos, st) <- get
     case T.find tok st of
       []       -> throwError [Delim tok pos]
       (p, _):_ -> p <$ shift (T.length p + T.length tok)

satisfy :: (Char -> Bool) -> Parser Text
satisfy f =
  do (pos, st) <- get
     case T.takeWhile f st of
       ""  -> throwError [Satisfy pos]
       tok -> tok <$ shift (T.length tok)

