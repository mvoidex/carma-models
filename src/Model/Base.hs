{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Base (
    Field(..),
    emptyField, mkField,
    Meta(..), FieldInfo(..), ModelInfo,
    field, field_,
    memptyIso, mappendIso,
    FieldSource, ModelSource(..),
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

import Data.Serialization
import Data.Serialization.Dictionary
import Data.Serialization.Text.Print
import Data.Serialization.Text.Attoparsec
import Data.Serialization.JSON.Aeson
import Data.Serialization.JSON.Aeson as S (toJSON, fromJSON)
import Data.Serialization.Postgresql
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
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
    mappend _ r = r

-- | Field with documentation
data Meta m v a = Meta {
    metaDict :: Dictionarable Text v a,
    metaPg :: Postgresable a,
    metaInfo :: m }

instance Monoid m => Combine (Meta m v) where
    (Meta ld lp li) .**. (Meta rd rp ri) = Meta (ld .**. rd) (lp .**. rp) (mappend li ri)
    (Meta ld lp li) .++. (Meta rd rp ri) = Meta (ld .++. rd) (lp .++. rp) (mappend li ri)
    (Meta ld lp li) .+. (Meta rd rp ri) = Meta (ld .+. rd) (lp .+. rp) (mappend li ri)
    iso <<>> (Meta d p i) = Meta (iso <<>> d) (iso <<>> p) i
    (Meta d p i) .:. iso = Meta (d .:. iso) (p .:. iso) i
    (Meta d p i) .*>> f = Meta (d .*>> (metaDict . f)) (p .*>> (metaPg . f)) i
    pures x = Meta (pures x) (pures x) mempty
    fails = Meta fails fails mempty

-- | Field meta information
data FieldInfo = FieldInfo {
    fieldDescription :: Text }

-- | Meta model information
type ModelInfo = [(Text, Maybe FieldInfo)]

-- | Field serializer
fieldMeta :: (FieldSource v, DictionaryValue v a, FromField a, ToField a) => Text -> i -> Meta [(Text, i)] v (Field a)
fieldMeta name meta = Meta (Iso Field fieldValue <<>> try (entry name dictionaryValue)) (Iso Field fieldValue <<>> pgField) [(name, meta)]

-- | Field serializer
field :: (FieldSource v, DictionaryValue v a, FromField a, ToField a) => Text -> i -> Meta [(Text, Maybe i)] v (Field a)
field name meta = fieldMeta name (Just meta)

-- | Field serializer with no meta info
field_ :: (FieldSource v, DictionaryValue v a, FromField a, ToField a) => Text -> Meta [(Text, Maybe i)] v (Field a)
field_ name = fieldMeta name Nothing

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
    dictionaryValue = recode $ textual $ fmap T.decodeUtf8 P.takeByteString

instance DictionaryValue ByteString Double where
    dictionaryValue = recode $ textual P.double

instance DictionaryValue ByteString String where
    dictionaryValue = recode $ textual $ fmap (T.unpack . T.decodeUtf8) P.takeByteString

-- | 'Model' class, defines way to serialize data
class Monoid m => Model m where
    asDict ::
        (FieldSource v,
        DictionaryValue v Int,
        DictionaryValue v Text,
        DictionaryValue v Double,
        DictionaryValue v String)
        => Meta ModelInfo v m

-- | Dummy asDict' to fix type
asDict' :: Model m => Meta ModelInfo ByteString m
asDict' = asDict

-- | Default instance for Aeson
instance Model m => A.ToJSON m where
    toJSON = S.toJSON jsonModel

-- | Default instance for Aeson
instance Model m => A.FromJSON m where
    parseJSON = S.fromJSON jsonModel

-- | Default instance for Postgresql
instance Model m => FromRow m where
    fromRow = rowParser $ metaPg asDict'

-- | Default instance for Postgresql
instance Model m => ToRow m where
    toRow = rowWriter $ metaPg asDict'

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
modelInfo v = metaInfo (asDict_ v) where
    asDict_ :: Model m => m -> Meta ModelInfo ByteString m
    asDict_ _ = asDict

-- | Update value with partial data
update :: Monoid m => m -> Partial m -> m
update v (Partial x) = v `mappend` x

-- | Encode partial data
encodePartial :: (ModelSource s, Model m) => Partial m -> Either String s
encodePartial = encodeModel . partial

-- | Decode partial data
decodePartial :: (ModelSource s, Model m) => s -> Either String (Partial m)
decodePartial = fmap Partial . decodeModel
