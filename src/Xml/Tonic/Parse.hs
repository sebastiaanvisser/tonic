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
, parse

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

-- | Parse a lazy text into an XML tree.
--
-- The parser is very forgiving and will never fail. When the input is a valid
-- XML document the output XML tree will be a correct representation. When the
-- input is not valid XML the parser will use some cleaver heuristics to try to
-- build an appropriate XML tree as well. Not following the specification by
-- not failing on invalid documents gives us the benefit of fast online parsing
-- and allows us to process documents that have potential syntax or encoding
-- errors.

xml :: T.Text -> X.Xml
xml = parse (asMany node)

nodes :: Parser [X.Node]
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

attributeList :: Parser [X.Attribute]
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

-- | A very simple parser context for parsers that cannot fail. Because no
-- failure is possible by default we can do without any 'MonadPlus' and
-- 'Alternative' instances, which allows us to perform lazy online parsing.
-- Besides an 'Applicative' instance also a 'Monad' instance is provided,
-- because the open and close tags XML requires context-sensitive parsing.
--
-- When some specific parser actually has the ability to fail (by not consume
-- any input) it can make this explicit by using a 'Maybe' result value.

newtype Parser a = P { runP :: T.Text -> (T.Text, a) }
  deriving Functor

-- | Run a parser on an input text an return both the unconsumed remnant text
-- and the result value.

runParser :: Parser a -> T.Text -> (T.Text, a)
runParser = runP

-- | Run a parser on an input text and just return the result value.

parse :: Parser a -> T.Text -> a
parse p = snd . runP p

instance Applicative Parser where
  pure a  = P (\t -> (t, a))
  a <*> b = P (\t -> let (u, x) = runP a t; (v, y) = runP b u in (v, x y))

instance Monad Parser where
  return a = P (\t -> (t, a))
  a >>= b  = P (\t -> let (u, x) = runP a t in runP (b x) u)

-- | Consume the input text as long as the predicate holds. When no input can
-- be consumed the parser fails.

while :: (Char -> Bool) -> Parser (Maybe T.Text)
while f = P $ \i ->
  let v = T.takeWhile f i
  in if T.null v
     then (i, Nothing)
     else (T.drop (T.length v) i, Just v)

-- | Consume the input text until the specified token is encountered. The token
-- itself is consumed but not contained in the result.

until :: T.Text -> Parser T.Text
until t = P $ \i ->
  case T.find t i of
    []       -> ("", i)
    (v, r):_ -> (T.drop (T.length t) r, v)

-- | Parse a single token, when it succeeds continue with the first parser,
-- when then token can not be recognized continue with the second parser.

token :: T.Text -> Parser a -> Parser a -> Parser a
token t p q = P $ \i ->
  case T.stripPrefix t i of
    Just j -> runP p j
    _      -> runP q i

-- | Repeatedly apply a parsers until it fails.

asMany :: Parser (Maybe a) -> Parser [a]
asMany p = P $ \i ->
  case runP p i of
    (_, Nothing) -> (i, [])
    (j, Just a)  -> let (k, as) = runP (asMany p) j
                    in (k, a:as)

