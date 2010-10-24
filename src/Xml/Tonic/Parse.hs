{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , GADTs
  , ViewPatterns
  #-}
module Xml.Tonic.Parse
( 
  -- * Top level XML parser.
  xml
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Text.Lazy (Text)
import Prelude hiding (until)
import Xml.Tonic.Types
import qualified Data.Text.Lazy as T

xml :: Text -> Xml [Node]
xml = runParser nodeSet

node :: Parser (Maybe (Xml Node))
node = token "<!--"      (Just . Comment               <$> until "-->")
     . token "<![CDATA[" (Just . CData                 <$> until "]]>")
     . token "<!"        (Just . Doctype               <$> until ">")
     . token "<?"        (Just . ProcessingInstruction <$> until "?>")
     $ token "<"         element
                         text

text :: Parser (Maybe (Xml Node))
text = fmap Text <$> while (/= '<')

element :: Parser (Maybe (Xml Node))
element = runMaybeT $
  do tag <- MaybeT (while (not . (`elem` " \r\n/>")))
     lift $
       do space
          a <- attributeList
          s <- self
          c <- if s then nodeSet <* close tag else pure (NodeSet [])
          return (Element (QualifiedName tag) a c)

close :: Text -> Parser ()
close w = token ("</" `T.append` w `T.append` ">") (pure ()) (pure ())

self :: Parser Bool
self = token ">"  (pure True)
     $ token "/>" (pure False)
                  (pure False)

nodeSet :: Parser (Xml [Node])
nodeSet = NodeSet <$> asMany node

attributeList :: Parser (Xml [Attr])
attributeList = AttributeList <$> asMany (space *> attribute)

attribute :: Parser (Maybe (Xml Attr))
attribute = runMaybeT $
  do k <- MaybeT (while (not . (`elem` " \r\n=>/")))
     v <- lift value
     return (Attribute (QualifiedName k) v)

value :: Parser Text
value = fromMaybe "" <$> token "=" (Just <$> doubleQuoted (singleQuoted unquoted)) (pure Nothing)
  where doubleQuoted = token "\"" (until "\"")
        singleQuoted = token "\'" (until "\'")
        unquoted     = fromMaybe "" <$> while (not . (`elem` " \r\n>/"))

space :: Parser ()
space = () <$ while isSpace

-------------------------------------------------------------------------------

newtype Parser a = P { runP :: Text -> (Text, a) }
  deriving Functor

runParser :: Parser b -> Text -> b
runParser p = snd . runP p

instance Applicative Parser where
  pure a  = P (\t -> (t, a))
  a <*> b = P (\t -> let (u, x) = runP a t; (v, y) = runP b u in (v, x y))

instance Monad Parser where
  return a = P (\t -> (t, a))
  a >>= b  = P (\t -> let (u, x) = runP a t in runP (b x) u)

while :: (Char -> Bool) -> Parser (Maybe Text)
while f = P $ \i ->
  let v = T.takeWhile f i
  in if T.null v
     then (i, Nothing)
     else (T.drop (T.length v) i, Just v)

until :: Text -> Parser Text
until t = P $ \i ->
  case T.find t i of
    []       -> ("", i)
    (v, r):_ -> (T.drop (T.length t) r, v)

token :: Text -> Parser a -> Parser a -> Parser a
token t p q = P $ \i ->
  case i of
    (T.stripPrefix t -> Just j) -> runP p j
    _                           -> runP q i

asMany :: Parser (Maybe a) -> Parser [a]
asMany p = P $ \i ->
  case runP p i of
    (_, Nothing) -> (i, [])
    (j, Just a)  -> let (k, as) = runP (asMany p) j
                    in (k, a:as)

