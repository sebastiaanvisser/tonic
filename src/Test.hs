module Test where

import Parser
import Prelude hiding (until)
import XmlParser
import qualified Data.Text.IO as T

main :: IO ()
main =
  do html <- T.readFile "test.html"
     print (parse nodes html)

