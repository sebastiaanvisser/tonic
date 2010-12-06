{-# LANGUAGE GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Arrow.ArrowList
import Control.Arrow.List
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.), elem)
import Xml.Tonic.Arrow
import Xml.Tonic.Types (Xml, Node(..))
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import qualified Xml.Tonic.Parse as Parse
import qualified Xml.Tonic.Print as Print

main :: IO ()
main =
  do f <- T.readFile "tests/files/64x.xml"
--      mapM_ T.putStrLn (transform myTransformation f)
     print $ length (transform myTransformation f)
  where
    transform tr = runListArrow tr . Parse.xml
    myTransformation = deep (elem "a") . isElem . unlist

