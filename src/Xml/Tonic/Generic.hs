{-# LANGUAGE
    Arrows
  , DeriveGeneric
  , DefaultSignatures
  , TypeOperators
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
module Xml.Tonic.Generic
( Parser
, Printer
, Xml (..)
, toXmlText
, fromXmlText
)
where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.String
import Data.Text.Lazy (Text, pack, unpack)
import GHC.Generics
import Xml.Tonic.Parse
import Xml.Tonic.Print
import Xml.Tonic.Types (Node, Nodes, Element(..), Node(..))

import qualified Data.Text.Lazy  as T
import qualified Xml.Tonic.Types as X

-- import Debug.Trace
-- tt x = trace (show x) x

-------------------------------------------------------------------------------

data Expected
  = ExpectedElement Nodes Text
  | ExpectedText    Nodes Text
  | ExpectedPrim    Text  Text
  deriving Show

instance Error Expected

errorText :: Expected -> Text
errorText (ExpectedElement   ns el) = T.concat ["Expected element '", el, "' but found: ", printer (take 1 ns)]
errorText (ExpectedText      ns ty) = T.concat ["Expected '", ty, "' but found: ", printer (take 1 ns)]
errorText (ExpectedPrim      ns ty) = T.concat ["Expected primitive '", ty, "' but found: ", ns]

type Printer a = a -> Nodes
type Parser  a = ErrorT Expected (StateT Nodes Identity) a

printElem :: GXml f => Text -> Printer (f a)
printElem con
  | T.null con = gtoXml
  | otherwise  = return . ElementNode . Element con [] . gtoXml

showPrinter :: Show a => a -> Nodes
showPrinter = return . fromString . show

parseElem :: GXml f => Text -> Parser (f a)
parseElem con
  | T.null con = gfromXml
  | otherwise =
    do x <- get;
       case x of
         ElementNode (Element n _ cs) : r | n == con
           -> put cs <* put r *> gfromXml
         _ -> throwError (ExpectedElement x con)

parseText :: Text -> Parser Text
parseText msg =
  do x <- get
     case x of
       TextNode (X.Text t) : r
         -> t <$ put r
       _ -> throwError (ExpectedText x msg)

parseList :: Xml a => Parser [a]
parseList = 
  do x <- (return <$> fromXml)
       `catchError` const (return [])
     case x of
       [] -> return x
       _  -> (x ++) <$> parseList

readParser :: Read a => Text -> Parser a
readParser msg =
  do v <- parseText msg
     case reading v of
       []  -> throwError (ExpectedPrim v msg)
       i:_ -> return i

-------------------------------------------------------------------------------

-- Type class for conversion from XML to Haskell datatypes and vice versa.

class Show a => Xml a where

  default toXml   :: (Generic a, GXml (Rep a)) => Printer a
  default fromXml :: (Generic a, GXml (Rep a)) => Parser  a

  toXml :: Printer a
  toXml = gtoXml . from

  fromXml :: Parser a
  fromXml = to <$> gfromXml

toXmlText :: Xml a => a -> Text
toXmlText = printer . toXml

fromXmlText :: Xml a => Text -> Either Text a
fromXmlText x
  = either (Left . errorText) Right
  . runIdentity
  . flip evalStateT
    (parser x)
  $ runErrorT fromXml

-- Some Xml instance for primitive types.

instance Xml () where
  toXml   = const []
  fromXml = return ()

instance Xml Text where
  toXml   = return . TextNode . X.Text
  fromXml = parseText "text"

instance Xml a => Xml [a] where
  toXml   = concatMap toXml
  fromXml = parseList

instance (Show a, Xml a) => Xml (Maybe a) where
  toXml   = maybe [] toXml
  fromXml = Just <$> fromXml <|> pure Nothing

instance (Xml a, Xml b) => Xml (a, b) where
  toXml (a, b) = toXml a ++ toXml b
  fromXml = (,) <$> fromXml <*> fromXml

instance (Xml a, Xml b) => Xml (Either a b) where
  toXml = either toXml toXml
  fromXml = Left <$> fromXml <|> Right <$> fromXml

instance Xml Bool where
  toXml True  = ["true"]
  toXml False = ["false"]
  fromXml = ((`Prelude.elem` ["true", "yes", "on"]) . T.toLower) <$> parseText "boolean"

instance Xml Int where
  toXml   = showPrinter
  fromXml = readParser "integer"

instance Xml Float where
  toXml   = showPrinter
  fromXml = readParser "float"

reading :: Read b => Text -> [b]
reading = map fst . filter (null . snd) . reads . unpack

-- Type class for generic conversion from XML to Haskell datatypes and vice versa.

class GXml f where
  gtoXml   :: Printer (f a)
  gfromXml :: Parser  (f a)

instance GXml U1 where
  gtoXml   = const []
  gfromXml = return U1

instance (GXml a, GXml b) => GXml (a :*: b) where
  gtoXml (x :*: y) = gtoXml x ++ gtoXml y
  gfromXml = (:*:) <$> gfromXml <*> gfromXml

instance (GXml a, GXml b) => GXml (a :+: b) where
  gtoXml (L1 l) = gtoXml l
  gtoXml (R1 r) = gtoXml r
  gfromXml = L1 <$> gfromXml <|> R1 <$> gfromXml

instance GXml a => GXml (M1 D c a) where
  gtoXml   = gtoXml . unM1
  gfromXml = M1 <$> gfromXml

instance (Constructor c, GXml a) => GXml (M1 C c a) where
  gtoXml   = printElem (pack (conName (undefined :: M1 C c a b))) . unM1
  gfromXml = M1 <$> parseElem (pack (conName (undefined :: M1 C c a b)))

instance (Selector s, GXml a) => GXml (M1 S s a) where
  gtoXml   = printElem (pack (selName (undefined :: M1 S s a b))) . unM1
  gfromXml = M1 <$> parseElem (pack (selName (undefined :: M1 S s a b)))

instance Xml a => GXml (K1 i a) where
  gtoXml   = toXml . unK1
  gfromXml = K1 <$> fromXml

{-

data Wind =
    North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance Xml Wind

winds :: Text
winds = toXmlText ([minBound .. maxBound] :: [Wind])

xxx :: Either Text [Wind]
xxx = fromXmlText winds

newtype X = X (Maybe Bool)
  deriving (Generic, Show)

instance Xml X

data Person =
    Person
    { name :: Text
    , pass :: Text
    , age  :: (Int, Bool)
    , xs   :: [X]
    }
  deriving (Generic, Show)

instance Xml Person

myPerson :: Person
myPerson = Person "sebas" "secret" (28, True) [X Nothing, X (Just False), X Nothing, X (Just True)]

myTest :: Text
myTest = toXmlText myPerson

myTest2 :: Either Text Person
myTest2 = fromXmlText myTest

-}

