{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Arrow.ArrowList
import Control.Arrow.List
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.), elem)
import Xml.Tonic
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
  do T.readFile "../tests/test.html" >>= T.putStrLn . transform id
     T.readFile "../tests/test.html" >>= putStrLn . show . destruct id

