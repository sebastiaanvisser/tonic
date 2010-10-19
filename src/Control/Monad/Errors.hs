{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  #-}
module Control.Monad.Errors where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid

instance Monoid a => Monoid (Either a b) where
  mempty = Left mempty
  Right l `mappend` _       = Right l
  _       `mappend` Right r = Right r
  Left  l `mappend` Left  r = Left (l `mappend` r)

newtype ErrorsT e m a = ErrorsT { runErrorsT :: m (Either e a) }

instance Functor m => Functor (ErrorsT e m) where
  fmap f = ErrorsT . fmap (fmap f) . runErrorsT

instance Applicative m => Applicative (ErrorsT e m) where
  pure = ErrorsT . pure . Right
  a <*> b = ErrorsT ((<*>) <$> runErrorsT a <*> runErrorsT b)

instance Monad m => Monad (ErrorsT e m) where
  return = ErrorsT . return . Right
  a >>= b = ErrorsT $
    do v <- runErrorsT a
       case v of
         Left  l -> return (Left l)
         Right r -> runErrorsT (b r)

instance (Applicative m, Monoid e) => Alternative (ErrorsT e m) where
  empty = ErrorsT (pure (Left mempty))
  a <|> b = ErrorsT (mappend <$> runErrorsT a <*> runErrorsT b)

instance (Monad m, Monoid e) => MonadPlus (ErrorsT e m) where
  mzero = ErrorsT (return mempty)
  a `mplus` b = ErrorsT (liftM2 mappend (runErrorsT a) (runErrorsT b))

instance Monad m => MonadError e (ErrorsT e m) where
  throwError = ErrorsT . return . Left
  catchError a c = ErrorsT $
    do v <- runErrorsT a
       case v of
         Left  l -> runErrorsT (c l)
         Right r -> return (Right r)

