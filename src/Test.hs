module Test where

import Prelude hiding (until)
import Xml.Tonic.Parse as Parse
import Xml.Tonic.Print as Print
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
  do f <- T.readFile "../tests/test.html"
     let parsed = Parse.xml f
     T.putStrLn (Print.pretty parsed)
     _ <- getLine

     g <- T.readFile "../tests/xml.html"
     let parsed1 = Parse.xml g
     print parsed1
--      putStrLn "----------------------------------------------"
--      T.writeFile "../tests/out.html" (Print.pretty parsed)
