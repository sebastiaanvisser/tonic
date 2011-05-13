{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}
module Main where

import Data.Monoid
import Control.Category
import Control.Monad
import Control.DeepSeq
import Criterion.Config
import Criterion.Main
import Data.Time.Clock
import System.Environment
import System.IO
import Prelude hiding (id, (.), elem)

import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as T

import Xml.Tonic

main :: IO ()
main =
--     benchmarks "files/0x4.xml"
    forM_ [0..9::Int] (const (profile parser "files/0x4.xml"))

--   do args <- getArgs
--      case args of
--        ["benchmarks",        file] -> benchmarks file
--        ["profile", "parser", file] -> forM_ [0..9::Int] (const (profile parser file))
--        ["profile", "anchor", file] -> profile allAnchors file
--        _                           -> putStrLn "error: unrecognised action, try 'benchmarks' or 'profile'."

allAnchors :: T.Text -> [Element]
allAnchors = destruct (deep (elem "a") . isElem)

profile :: NFData a => (T.Text -> a) -> FilePath -> IO ()
profile action file =
  do start <- getCurrentTime
     txt <- T.readFile file
     txt `deepseq` return ()
     putStr "profiling... "; hFlush stdout
     let xml = action txt
     xml `deepseq` return ()
     stop  <- getCurrentTime
     let duration = diffUTCTime stop start
     putStrLn "ok"
     putStrLn ("Characters:     " ++ show (T.length txt `div` 1024) ++ "k")
     putStrLn ("Duration:       " ++ show duration)
     putStrLn ("Chars per sec:  " ++ show (round (fromIntegral (T.length txt) / duration) `div` 1024 :: Int) ++ "k")

benchmarks :: FilePath -> IO ()
benchmarks file =
  do txt <- readXml file
     let xml = parser txt
     xml `deepseq` return ()
     defaultMainWith cfg (return ())
       [ bench "parser"         $ nf parser             txt
       , bench "printer"        $ nf printer            xml
       , bench "parser/printer" $ nf (printer . parser) txt
       , bench "printer/parser" $ nf (parser . printer) xml
       ]
  where cfg = defaultConfig
                { -- cfgPlot    = M.singleton KernelDensity (PNG 800 600)
                  cfgSamples = Last (Just 100)
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

