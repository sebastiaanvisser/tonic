module Test where

import Prelude hiding (until)
import Xml.Tonic.Parse as Parse
import Xml.Tonic.Print as Print
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
  do f <- T.readFile "../tests/xml.html"
     let parsed = Parse.xml f
     T.writeFile "../tests/out.html" (Print.pretty parsed)
