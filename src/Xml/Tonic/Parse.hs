{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , GADTs
  #-}
module Xml.Tonic.Parse
(

-- * Top level XML parser.
  xml

-- * Individual parsers.
, nodes
, node
, text
, element
, close
, self
, attributeList
, attribute
, value
, space

-- * Parse monad.
, Parser 
, runParser

-- * Primitive parsers.
, while
, until
, token
, asMany
)
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Prelude hiding (until)
import qualified Data.Text.Lazy as T
import qualified Xml.Tonic.Types as X

xml :: T.Text -> X.Xml
xml = runParser (asMany node)

nodes :: Parser X.Nodes
nodes = asMany node

node :: Parser (Maybe X.Node)
node = token "<!--"      (Just . X.CommentNode               . X.Comment               <$> until "-->")
     . token "<![CDATA[" (Just . X.CDataNode                 . X.CData                 <$> until "]]>")
     . token "<!"        (Just . X.DoctypeNode               . X.Doctype               <$> until ">")
     . token "<?"        (Just . X.ProcessingInstructionNode . X.ProcessingInstruction <$> until "?>")
     $ token "<"         element
                         text

text :: Parser (Maybe X.Node)
text = fmap (X.TextNode . X.Text) <$> while (/= '<')

element :: Parser (Maybe X.Node)
element =
  do tag <- while (not . (`elem` " \r\n/>"))
     case tag of
       Nothing -> return Nothing
       Just t  ->
         do space
            a <- attributeList
            s <- self
            c <- if s then nodes <* close t else pure []
            c `seq` return (Just (X.ElementNode (X.Element t a c)))

close :: T.Text -> Parser ()
close w = token ("</" `T.append` w `T.append` ">") (pure ()) (pure ())

self :: Parser Bool
self = token ">"  (pure True)
     $ token "/>" (pure False)
                  (pure False)

attributeList :: Parser X.Attributes
attributeList = asMany (space *> attribute)

attribute :: Parser (Maybe X.Attribute)
attribute =
  do k <- while (not . (`elem` " \r\n=>/"))
     case k of
       Nothing -> return Nothing
       Just key ->
         do v <- space *> value
            return (Just (X.Attribute key v))

value :: Parser T.Text
value = fromMaybe "" <$> token "=" (space *> (Just <$> doubleQuoted (singleQuoted unquoted))) (pure Nothing)
  where doubleQuoted = token "\"" (until "\"")
        singleQuoted = token "\'" (until "\'")
        unquoted     = fromMaybe "" <$> while (not . (`elem` " \r\n>/"))

space :: Parser ()
space = () <$ while isSpace

-------------------------------------------------------------------------------

newtype Parser a = P { runP :: T.Text -> (T.Text, a) }
  deriving Functor

runParser :: Parser b -> T.Text -> b
runParser p = snd . runP p

instance Applicative Parser where
  pure a  = P (\t -> (t, a))
  a <*> b = P (\t -> let (u, x) = runP a t; (v, y) = runP b u in (v, x y))

instance Monad Parser where
  return a = P (\t -> (t, a))
  a >>= b  = P (\t -> let (u, x) = runP a t in runP (b x) u)

while :: (Char -> Bool) -> Parser (Maybe T.Text)
while f = P $ \i ->
  let v = T.takeWhile f i
  in if T.null v
     then (i, Nothing)
     else (T.drop (T.length v) i, Just v)

until :: T.Text -> Parser T.Text
until t = P $ \i ->
  case T.find t i of
    []       -> ("", i)
    (v, r):_ -> (T.drop (T.length t) r, v)

token :: T.Text -> Parser a -> Parser a -> Parser a
token t p q = P $ \i ->
  case T.stripPrefix t i of
    Just j -> runP p j
    _      -> runP q i

asMany :: Parser (Maybe a) -> Parser [a]
asMany p = P $ \i ->
  case runP p i of
    (_, Nothing) -> (i, [])
    (j, Just a)  -> let (k, as) = runP (asMany p) j
                    in (k, a:as)

