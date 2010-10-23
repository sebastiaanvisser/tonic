module Test where

import Prelude hiding (until)
import Xml.Lax.Parser
import qualified Data.Text.IO as T

main :: IO ()
main =
  do html <- T.readFile "test.html"
     print (parse html)

