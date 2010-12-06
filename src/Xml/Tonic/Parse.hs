{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , GADTs
  , TupleSections
  #-}
module Xml.Tonic.Parse
(

-- * Top level XML parser.
  xml

-- * Individual parsers.
, node
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

node :: Parser (Maybe X.Node)
node =
  token "<"
    ( token "!"
        ( token "--"      ((Just . X.CommentNode               . X.Comment              ) `fmap` until "-->")
        $ token "[CDATA[" ((Just . X.CDataNode                 . X.CData                ) `fmap` until "]]>")
                          ((Just . X.DoctypeNode               . X.Doctype              ) `fmap` until ">"))
      . token "?"         ((Just . X.ProcessingInstructionNode . X.ProcessingInstruction) `fmap` until "?>")
      $ element
    ) (fmap (X.TextNode . X.Text) `fmap` while (/= '<'))

element :: Parser (Maybe X.Node)
element =
  do tag <- while (not . (`elem` " \n/>"))
     case tag of
       Nothing -> return Nothing
       Just t  ->
         do _ <- space
            a <- attributeList
            s <- self
            c <- if s then
                    do ns <- asMany node
                       close t
                       return ns
                 else return []
            return (Just (X.ElementNode (X.Element t a c)))

close :: T.Text -> Parser ()
close w = token ("</" `T.append` w `T.append` ">") (return ()) (return ())

self :: Parser Bool
self = token ">"  (return True)
     $ token "/>" (return False)
                  (return False)

attributeList :: Parser [X.Attribute]
attributeList = asMany (space >> attribute)

attribute :: Parser (Maybe X.Attribute)
attribute =
  do k <- while (not . (`elem` " \n=>/"))
     case k of
       Nothing -> return Nothing
       Just key ->
         do v <- space >> value
            return (Just (X.Attribute key v))

value :: Parser T.Text
value = fromMaybe "" `fmap` token "=" (space >> (Just `fmap` doubleQuoted (singleQuoted unquoted))) (return Nothing)
  where doubleQuoted = token "\"" (until "\"")
        singleQuoted = token "\'" (until "\'")
        unquoted     = fromMaybe "" `fmap` while (not . (`elem` " \n>/"))

space :: Parser (Maybe T.Text)
space = while isSpace

-------------------------------------------------------------------------------

-- | A very simple parser context for parsers that cannot fail. Because no
-- failure is possible by default we can do without any 'MonadPlus' and
-- 'Alternative' instances, which allows us to perform lazy online parsing.
-- A 'Monad' instance is provided, because the open and close tags XML requires
-- context-sensitive parsing.
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

instance Monad Parser where
  return a = P (,a)
  a >>= b  = P (\t -> let (u, x) = runP a t in runP (b x) u)

-- | Consume the input text as long as the predicate holds. When no input can
-- be consumed the parser fails.

while :: (Char -> Bool) -> Parser (Maybe T.Text)
while f = P $ \i ->
  let (v, r) = T.span f i
  in if T.null v
     then (i, Nothing)
     else (r, Just v)

-- | Consume the input text until the specified token is encountered. The token
-- itself is consumed but not contained in the result.

until :: T.Text -> Parser T.Text
until t = P $ \i ->
  let (v, r) = T.breakOn t i
  in if T.null r
     then ("", i)
     else (T.drop (T.length t) r, v)

-- | Parse a single token, when it succeeds continue with the first parser,
-- when then token can not be recognized continue with the second parser.

token :: T.Text -> Parser a -> Parser a -> Parser a
token t p q = P $ \i ->
  case T.stripPrefix t i of
    Just j -> runP p j
    _      -> runP q i

-- | Repeatedly apply a parsers until it fails.

asMany :: Parser (Maybe a) -> Parser [a]
asMany p = go
 where go = do x <- p
               case x of
                 Nothing -> return []
                 Just a  -> do as <- go
                               return (a:as)

