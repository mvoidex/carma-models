{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Model.Base (
    Field(..),
    emptyField, mkField,
    field,
    memptyIso, mappendIso,
    FieldSource(..), ModelSource(..),
    Model(..),
    Partial(..),
    jsonModel, redisModel,
    modelInfo,
    update,
    encodePartial, decodePartial,

    module Data.Monoid,
    (.**.), (.:.)
    ) where

import qualified Data.Map as M
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Function (fix)

import Data.Serialization
import Data.Serialization.Dictionary
import Data.Serialization.Text.Print
import Data.Serialization.Text.Attoparsec
import Data.Serialization.JSON.Aeson
import Data.Serialization.JSON.Aeson as S (toJSON, fromJSON)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString.Char8 as P

-- | Model field
newtype Field a = Field { fieldValue :: Maybe a }
    deriving (Eq, Ord, Read, Show)

-- | Empty field
emptyField :: Field a
emptyField = Field Nothing

-- | Field value
mkField :: a -> Field a
mkField = Field . Just

instance Monoid (Field a) where
    mempty = Field Nothing
    mappend l (Field Nothing) = l
    mappend l r = r

-- | Field with documentation
data Meta m v a = Meta {
    metaDict :: Dictionarable Text v a,
    metaInfo :: m }

instance Monoid m => Combine (Meta m v) where
    (Meta ld li) .**. (Meta rd ri) = Meta (ld .**. rd) (mappend li ri)
    (Meta ld li) .++. (Meta rd ri) = Meta (ld .++. rd) (mappend li ri)
    (Meta ld li) .+. (Meta rd ri) = Meta (ld .+. rd) (mappend li ri)
    iso <<>> (Meta d i) = Meta (iso <<>> d) i
    (Meta d i) .:. iso = Meta (d .:. iso) i
    (Meta d i) .*>> f = Meta (d .*>> (metaDict . f)) i
    pures x = Meta (pures x) mempty
    fails = Meta fails mempty

-- | Meta model information
type ModelInfo = M.Map Text Text

-- | Field serializer
field :: (FieldSource v, DictionaryValue v a) => Text -> Text -> Meta ModelInfo v (Field a)
field name desc = Meta (Iso Field fieldValue <<>> try (entry name dictionaryValue)) (M.singleton name desc)

-- | Monoid by iso
-- Used in definition of 'Monoid' for 'Model'

memptyIso :: (Monoid b) => Iso a b -> a
memptyIso i = comorph i mempty

mappendIso :: (Monoid b) => Iso a b -> a -> a -> a
mappendIso i l r = comorph i $ mappend (morph i l) (morph i r)

-- | Source of primitive field, no members, just for doc
class FieldSource s where

-- | Two sources: JSON-value and Map value
instance FieldSource A.Value
instance FieldSource ByteString

-- | Source of 'Model'
class ModelSource s where
    encodeModel :: Model m => m -> Either String s
    decodeModel :: Model m => s -> Either String m

-- | JSON source
instance ModelSource ByteString where
    encodeModel = encode (json <~> jsonModel)
    decodeModel = decode (json <~> jsonModel)

-- | Redis source
instance ModelSource (M.Map ByteString ByteString) where
    encodeModel = encode redisModel
    decodeModel = decode redisModel

-- | Implementations for encoding/decoding primitives info ByteString
type Textual a = SerializableT ByteString Print Atto a

textual :: (Show a) => P.Parser a -> Textual a
textual p = serializable printShow (atto p)

instance DictionaryValue ByteString Int where
    dictionaryValue = recode $ textual P.decimal

instance DictionaryValue ByteString Text where
    dictionaryValue = recode $ textual $ fmap T.decodeUtf8 $ P.takeByteString

instance DictionaryValue ByteString Double where
    dictionaryValue = recode $ textual P.double

instance DictionaryValue ByteString String where
    dictionaryValue = recode $ textual $ fmap (T.unpack . T.decodeUtf8) $ P.takeByteString

-- | 'Model' class, defines way to serialize data
class Monoid m => Model m where
    asDict ::
        (FieldSource v,
        DictionaryValue v Int,
        DictionaryValue v Text,
        DictionaryValue v Double,
        DictionaryValue v String)
        => Meta ModelInfo v m

-- | Default instance for Aeson
instance Model m => A.ToJSON m where
    toJSON = S.toJSON jsonModel

-- | Default instance for Aeson
instance Model m => A.FromJSON m where
    parseJSON = S.fromJSON jsonModel

-- | Dummy type to show, that model is not value itself, it's just partial value
newtype Partial m = Partial { partial :: m }
    deriving (Eq, Ord, Read, Show)

instance Monoid m => Monoid (Partial m) where
    mempty = Partial mempty
    mappend (Partial l) (Partial r) = Partial $ mappend l r

-- | Serialization into JSON
jsonModel :: Model m => Jsonable m
jsonModel = dict $ metaDict asDict

-- | Serialization into Map ByteString ByteString (Redis)
redisModel :: Model m => Dictionarable ByteString ByteString m
redisModel = rawDict <~> metaDict asDict where
    rawDict :: Dictionarable ByteString ByteString (M.Map Text ByteString)
    rawDict = Iso toDict fromDict <<>> anything
    toDict :: M.Map ByteString ByteString -> M.Map Text ByteString
    toDict = M.mapKeys T.decodeUtf8
    fromDict :: M.Map Text ByteString -> M.Map ByteString ByteString
    fromDict = M.mapKeys T.encodeUtf8

modelInfo :: Model m => m -> ModelInfo
modelInfo v = metaInfo (asDict' v) where
    asDict' :: Model m => m -> Meta ModelInfo ByteString m
    asDict' _ = asDict

-- | Update value with partial data
update :: Monoid m => m -> Partial m -> m
update v (Partial x) = v `mappend` x

-- | Encode partial data
encodePartial :: (ModelSource s, Model m) => Partial m -> Either String s
encodePartial = encodeModel . partial

-- | Decode partial data
decodePartial :: (ModelSource s, Model m) => s -> Either String (Partial m)
decodePartial = fmap Partial . decodeModel
