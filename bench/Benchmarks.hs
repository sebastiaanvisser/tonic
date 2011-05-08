{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}
module Main where

import Data.Monoid
import Control.DeepSeq
import Criterion.Config
import Criterion.Main

import qualified Criterion.MultiMap as M
import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as T

import Xml.Tonic

main :: IO ()
main =
  do txt <- readXml "files/0x10.xml"
     let xml = parser txt
     xml `deepseq` return ()
     defaultMainWith cfg (return ())
       [ bench "parser"         $ nf parser             txt
       , bench "printer"        $ nf printer            xml
       , bench "parser/printer" $ nf (printer . parser) txt
       , bench "printer/parser" $ nf (parser . printer) xml
       ]
  where cfg = defaultConfig
                { cfgPlot    = M.singleton KernelDensity (PNG 800 600)
                , cfgSamples = Last (Just 4)
                }

readXml :: FilePath -> IO T.Text
readXml file =
  do xml <- T.readFile file
     xml `deepseq` return ()
     putStrLn ("Read file from disk: " ++ show (T.length xml `div` 1024) ++ "k characters")
     return xml

-------------------------------------------------------------------------------
-- NFData instances for evaluating to normal form.

deriving instance NFData Text
deriving instance NFData CData
deriving instance NFData Comment 
deriving instance NFData Doctype
deriving instance NFData ProcessingInstruction

instance NFData Attribute where
  rnf (Attribute k v) = rnf k `seq` rnf v

instance NFData Element where
  rnf (Element n a c) = rnf n `seq` rnf a `seq` rnf c

instance NFData Node where
  rnf (ElementNode               a) = rnf a
  rnf (TextNode                  a) = rnf a
  rnf (CDataNode                 a) = rnf a
  rnf (CommentNode               a) = rnf a
  rnf (DoctypeNode               a) = rnf a
  rnf (ProcessingInstructionNode a) = rnf a

