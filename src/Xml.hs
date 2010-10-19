{-# LANGUAGE OverloadedStrings #-}
module Xml where

import Control.Applicative
import Data.Text
import Parser

test = runParser pp "hallo doei;hall oija sodij asiodj asidoj ;  Xyy aoisj aosidji;          yy etskipaapaap;      yy aapkipnietsZXXinietsaaphallo allemaal"

pp = (,,) <$> delim ";" <*> satisfy (/='X') <*> token "Xyy a"

