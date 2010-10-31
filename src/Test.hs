{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Category
import Control.Arrow.List
import Xml.Tonic.Arrow
import Prelude hiding (id, (.), elem)

import qualified Xml.Tonic.Parse as Parse
import qualified Xml.Tonic.Print as Print
import qualified Data.Text.Lazy.IO as T


main :: IO ()
main =
  do
     f <- T.readFile "../tests/test.html"
     let parsed = Parse.xml f
     T.putStrLn (Print.pretty parsed)
     putStrLn "-------------------------"

     mapM_ (T.putStrLn . Print.pretty) (runListArrow tr parsed)

     where tr = parseXml . (arr (const "<span class=xxx></span>"))
                <+> toCData . printXml . deepWhenNot (elem "xs:second") (elem "aap") . nodes

