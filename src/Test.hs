{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Prelude hiding (id, (.), elem)
import qualified Data.Text.Lazy.IO as T
import qualified Xml.Tonic.Parse as Parse
import qualified Xml.Tonic.Print as Print

main :: IO ()
main =
  do f <- T.readFile "../tests/16x.xml"
     let parsed = Parse.xml f
     T.putStrLn (Print.xml parsed)
