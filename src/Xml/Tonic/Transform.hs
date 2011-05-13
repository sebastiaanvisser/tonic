{-# LANGUAGE
    TypeOperators
  , OverloadedStrings
  #-}
module Xml.Tonic.Transform
( (:~>)
, construct
, destruct
, transform
)
where

import Control.Arrow.List
import Control.Category
import Data.Text.Lazy (Text)
import Prelude hiding ((.), id)
import Xml.Tonic.Arrow
import Xml.Tonic.Types (Node)

import qualified Data.Text.Lazy as T

-- | Prettier way to write down the ListArrow type.

type a :~> b = ListArrow a b

-- | Construct an XML representation from some value using a list arrow.

construct :: (a :~> Node) -> a -> Text
construct tr = T.concat . runListArrow (printXml . tr)

-- | Destruct an XML representation to some values using a list arrow.
--
-- For example, if you want to select the 'src' attributes for all the 'img'
-- tags in your XML document you can use the following construction:
--
--   > sources :: Text -> [Text]
--   > sources = destruct (deep (attr "src" . elem "img") . isElem)
--   >
--   >   ghci> :set -XOverloadedStrings 
--   >   ghci> sources "<div><img src=a.png/><a><img src=b.jpg/></a></div>"
--   >   ["a.png","b.jpg"]

destruct :: (Node :~> a) -> Text -> [a]
destruct tr = runListArrow (tr . parseXml)

-- | Transform an XML representation using a list arrow.

transform :: (Node :~> Node) -> Text -> Text
transform tr = T.concat . runListArrow (printXml . tr . parseXml)

-- renameULs :: Text -> Text
-- renameULs = transform (elemNode . processTopDownWhen id remove . isElem)
--   where remove = filterA (notA (isA (=="ul") . name))

-- test = renameULs "<div><ul><li>hallo</li><li/></ul><p><ul/></p></div>"
-- test = renameULs "<div><ol><li>hallo</li><li/></ol><p><ol/></p></div>"


