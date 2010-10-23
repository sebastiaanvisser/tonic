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
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as T

type P a = StateT Text (ReaderT [Text] (MaybeT Identity)) a

noParse :: a
noParse = error "This is a bug, please report."

parse :: P a -> Text -> a
parse parser input
  = runIdentity
  . fmap (fromMaybe noParse)
  . runMaybeT
  . flip runReaderT []
  . flip evalStateT input
  $ parser

progress :: Int -> P ()
progress n = modify (T.drop n)

token :: Text -> P Text
token tok =
  do st <- get
     if isPrefixOf tok st
       then tok <$ progress (T.length tok)
       else fail ""

until :: Text -> P Text
until tok =
  do st <- get
     case T.find tok st of
       []       -> fail ""
       (p, _):_ -> p <$ progress (T.length p + T.length tok)

while :: (Char -> Bool) -> P Text
while f =
  do st <- get
     case T.takeWhile f st of
       ""  -> fail ""
       tok -> tok <$ progress (T.length tok)

option :: Alternative f => a -> f a -> f a
option d p = p <|> pure d

