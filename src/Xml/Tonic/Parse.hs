{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , DeriveFunctor
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , GADTs
  #-}
module Xml.Tonic.Parse (parser) where

import Control.Monad
import Data.Char
import Data.Maybe
import Prelude hiding (until)

import qualified Data.Text.Lazy  as T
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

parser :: T.Text -> X.Xml
parser = parse nodes

nodes :: Parser [X.Node]
nodes = asMany (fakeClose >> node)

node :: Parser (Maybe X.Node)
node =
  peek '<'
    ( peek '!'
       ( tokenNS "--"      ((Just . X.CommentNode               . X.Comment              ) `liftM` until "-->")
       $ tokenNS "[CDATA[" ((Just . X.CDataNode                 . X.CData                ) `liftM` until "]]>")
                           ((Just . X.DoctypeNode               . X.Doctype              ) `liftM` until ">")
       )
       ( tokenNS "?"       ((Just . X.ProcessingInstructionNode . X.ProcessingInstruction) `liftM` until "?>")
       $ element
       )
    )
    text

text :: Parser (Maybe X.Node)
text = fmap (X.TextNode . X.Text) `liftM` while (/= '<')

element :: Parser (Maybe X.Node)
element =
  do space
     tag <- while (\x -> x /= ' ' && x /= '\r' && x /= '\n' && x /= '/' && x /= '>')
     case tag of
       Nothing -> return Nothing
       Just t  ->
         do a <- attributeList
            s <- self
            c <- if s then push t $
                    do ns <- nodes
                       _ <- close t
                       return ns
                 else return []
            return (Just (X.ElementNode (X.Element t a c)))


fakeClose :: Parser (Maybe ())
fakeClose = 
  do st <- stack
     try $
        do t <- token1 "</"
           n <- while (/= '>')
           _ <- token1 ">"
           return $
             case (t, n) of
               (Just _, Just m) | not (m `elem` st) -> Just ()
               _                                    -> Nothing

close :: T.Text -> Parser (Maybe T.Text)
close w = try $
  do a <- token1 "</"
     c <- token1 w
     d <- token1 ">"
     return (a >> c >> d)

self :: Parser Bool
self = token ">"  (return True)
     $ token "/>" (return False)
                  (return False)

attributeList :: Parser [X.Attribute]
attributeList = asMany attribute

attribute :: Parser (Maybe X.Attribute)
attribute =
  do space
     k <- while (\x -> x /= ' ' && x /= '\r' && x /= '\n' && x /= '=' && x /= '>' && x /= '/')
     case k of
       Nothing -> return Nothing
       Just key ->
         do v <- value
            return (Just (X.Attribute key v))

value :: Parser T.Text
value = fromMaybe "" `liftM` token "=" (Just `liftM` doubleQuoted (singleQuoted unquoted)) (return Nothing)
  where doubleQuoted = token "\"" (while1 (/= '\"'))
        singleQuoted = token "\'" (while1 (/= '\''))
        unquoted     = fromMaybe "" `liftM` while (\x -> x /= ' ' && x /= '\r' && x /= '\n' && x /= '=' && x /= '>' && x /= '/')

space :: Parser ()
space = while (\x -> x == ' ' || x == '\t' || x == '\r' || x == '\n') >> return ()

-------------------------------------------------------------------------------

-- | A very simple parser context for parsers that cannot fail. Because no
-- failure is possible by default we can do without any 'MonadPlus' and
-- 'Alternative' instances, which allows us to perform lazy online parsing.
--
-- When some specific parser actually has the ability to fail (by not consume
-- any input) it can make this explicit by using a 'Maybe' result value.

data R a = R T.Text a

newtype Parser a = P { runP :: [T.Text] -> T.Text -> R a }

-- | Run a parser on an input text an return both the unconsumed remnant text
-- and the result value.

runParser :: Parser a -> T.Text -> R a
runParser p = runP p []

try :: Parser (Maybe a) -> Parser (Maybe a)
try p = P $ \s i ->
  case runP p s i of
    R _ Nothing -> R i Nothing
    a           -> a

peek :: Char -> Parser a -> Parser a -> Parser a
peek c p q = P $ \s i ->
  case T.uncons i of
    Just (x, xs) | x == c -> runP p s xs
    _                     -> runP q s i

--   R t (fmap fst (T.uncons t))

push :: T.Text -> Parser a -> Parser a
push s p = P $ \ss -> runP p (s:ss)

stack :: Parser [T.Text]
stack = P $ \s i -> R i s

-- | Run a parser on an input text and just return the result value.

parse :: Parser a -> T.Text -> a
parse p i = let R _ r = runParser p i in r

instance Monad Parser where
  return a = P (\_ t -> R t a)
  a >>= b  = P (\s t -> let (R u x) = runP a s t in runP (b x) s u)

-- | Consume the input text as long as the predicate holds. When no input can
-- be consumed the parser fails.

while :: (Char -> Bool) -> Parser (Maybe T.Text)
while f = P $ \_ i ->
  case T.span f i of
    ("", _) -> R i Nothing
    (v,  r) -> R r (Just v)

while1 :: (Char -> Bool) -> Parser T.Text
while1 f = P $ \_ i ->
  case T.span f i of
    (v,  r) -> R (T.tail r) v

-- | Consume the input text until the specified token is encountered. The token
-- itself is consumed but not contained in the result.

until :: T.Text -> Parser T.Text
until t = P $ \_ i ->
  case T.breakOn t i of
    (_, "") -> R "" i
    (v, r ) -> R (T.drop (T.length t) r) v

-- | Parse a single token, when it succeeds continue with the first parser,
-- when then token can not be recognized continue with the second parser.

token :: T.Text -> Parser a -> Parser a -> Parser a
token t p q = P $ \s i ->
  case T.stripPrefix t (T.dropWhile isSpace i) of
    Just j -> runP p s j
    _      -> runP q s i

tokenNS :: T.Text -> Parser a -> Parser a -> Parser a
tokenNS t p q = P $ \s i ->
  case T.stripPrefix t i of
    Just j -> runP p s j
    _      -> runP q s i

token1 :: T.Text -> Parser (Maybe T.Text)
token1 t = P $ \_ i ->
  case T.stripPrefix t (T.dropWhile isSpace i) of
    Just j -> R j (Just t)
    _      -> R i Nothing


-- | Repeatedly apply a parsers until it fails.

asMany :: Parser (Maybe a) -> Parser [a]
asMany p = P $ \s i ->
  case runP p s i of
    R _ Nothing  -> R i []
    R j (Just a) -> let R k as = runP (asMany p) s j
                    in R k (a:as)

