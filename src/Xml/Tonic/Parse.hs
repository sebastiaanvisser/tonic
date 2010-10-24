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
module Xml.Tonic.Parse where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text.Lazy (Text)
import Prelude hiding (until)
import Xml.Tonic.Types
import qualified Data.Text.Lazy as T

xml :: Text -> Xml [Node]
xml = NodeSet . snd . runQ nodeSet

newtype Q a = Q { runQ :: Text -> (Text, a) }
  deriving Functor

instance Applicative Q where
  pure a  = Q (\t -> (t, a))
  a <*> b = Q (\t -> let (u, x) = runQ a t; (v, y) = runQ b u in (v, x y))

instance Monad Q where
  return a = Q (\t -> (t, a))
  a >>= b  = Q (\t -> let (u, x) = runQ a t in runQ (b x) u)

while :: (Char -> Bool) -> Q Text
while f = Q $ \i ->
  let v = T.takeWhile f i
  in (T.drop (T.length v) i, v)

until :: Text -> Q Text
until t = Q $ \i ->
  case T.find t i of
    []       -> ("", i)
    (v, r):_ -> (T.drop (T.length t) r, v)

node :: Q (Maybe (Xml Node))
node = Q $ \i ->
  case i of
    (T.stripPrefix "<!--"      -> Just j) -> Just . Comment               <$> runQ (until "-->") j
    (T.stripPrefix "<![CDATA[" -> Just j) -> Just . CData                 <$> runQ (until "]]>") j
    (T.stripPrefix "<!"        -> Just j) -> Just . Doctype               <$> runQ (until ">"  ) j
    (T.stripPrefix "<?"        -> Just j) -> Just . ProcessingInstruction <$> runQ (until "?>" ) j
    (T.stripPrefix "<"         -> Just j) -> runQ element j
    j                                     -> case runQ (while (/= '<')) j of
                                               (_, "") -> (i, Nothing)
                                               (k, t)  -> (k, Just (Text t))

element :: Q (Maybe (Xml Node))
element =
  do tag <- while (not . (`elem` " \r\n/>"))
     if T.null tag
       then return Nothing
       else
         do space
            a <- attributeList
            s <- self
            c <- if s
                   then nodeSet <* closeTag tag
                   else pure []
            return (Just (Element (QualifiedName tag) (AttributeList a) (NodeSet c)))

closeTag :: Text -> Q Text
closeTag w = Q $ \i ->
  case i of
    (T.stripPrefix ("</" `T.append` w `T.append` ">") -> Just j) -> (j, w)
    j                                                            -> (j, w)

self :: Q Bool
self = Q $ \i ->
  case i of
    (T.stripPrefix ">"  -> Just j) -> (j, True)
    (T.stripPrefix "/>" -> Just j) -> (j, False)
    j                              -> (j, False)

nodeSet :: Q [Xml Node]
nodeSet = asMany node

attributeList :: Q [Xml Attr]
attributeList = asMany (space *> attribute)

attribute :: Q (Maybe (Xml Attr))
attribute =
  do k <- while (not . (`elem` " \r\n=>/"))
     if T.null k
       then return Nothing
       else Just . Attribute (QualifiedName k) <$> optValue

optValue :: Q Text
optValue = Q $ \i ->
  case i of
    (T.stripPrefix "=" -> Just j) -> runQ value j
    j                             -> (j, "")

value :: Q Text
value = Q $ \i ->
  case i of
    (T.stripPrefix "\"" -> Just j) -> runQ (until "\"") j
    (T.stripPrefix "\'" -> Just j) -> runQ (until "\'") j
    j                              -> runQ (while (not . (`elem` " \r\n>/"))) j

space :: Q ()
space = () <$ while isSpace

asMany :: Q (Maybe a) -> Q [a]
asMany p = Q $ \i ->
  case runQ p i of
    (_, Nothing) -> (i, [])
    (j, Just a)  -> let (k, as) = runQ (asMany p) j
                    in (k, a:as)

