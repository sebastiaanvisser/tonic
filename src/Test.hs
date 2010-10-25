{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Main where

import Prelude hiding (until)
import Xml.Tonic.Parse as Parse
import Xml.Tonic.Print as Print
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
  do
     f <- T.readFile "../tests/test.html"
     let parsed = Parse.xml f
     T.putStrLn (Print.pretty parsed)
     _ <- getLine

     g <- T.readFile "../tests/single.html"
--      print (Parse.xml g)
     T.putStr (Print.asis (Parse.xml g))
--      T.writeFile "../tests/out.html" (Print.pretty parsed)
