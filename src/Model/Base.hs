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

-- | Entry serializer by convertible
field :: (FieldSource v, DictionaryValue v a) => Text -> Dictionarable Text v (Field a)
field name = Iso Field fieldValue <<>> try (entry name dictionaryValue)

-- | Monoid by iso
memptyIso :: (Monoid b) => Iso a b -> a
memptyIso i = comorph i mempty

mappendIso :: (Monoid b) => Iso a b -> a -> a -> a
mappendIso i l r = comorph i $ mappend (morph i l) (morph i r)

class FieldSource s where

instance FieldSource A.Value
instance FieldSource ByteString

class ModelSource s where
    encodeModel :: Model m => m -> Either String s
    decodeModel :: Model m => s -> Either String m

instance ModelSource ByteString where
    encodeModel = encode (json <~> jsonModel)
    decodeModel = decode (json <~> jsonModel)

instance ModelSource (M.Map ByteString ByteString) where
    encodeModel = encode redisModel
    decodeModel = decode redisModel

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

class Monoid m => Model m where
    asDict ::
        (FieldSource v,
        DictionaryValue v Int,
        DictionaryValue v Text,
        DictionaryValue v Double,
        DictionaryValue v String)
        => Dictionarable Text v m

instance Model m => A.ToJSON m where
    toJSON = S.toJSON jsonModel

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
jsonModel = dict asDict

-- | Serialization into Map ByteString ByteString
redisModel :: Model m => Dictionarable ByteString ByteString m
redisModel = rawDict <~> asDict where
    rawDict :: Dictionarable ByteString ByteString (M.Map Text ByteString)
    rawDict = Iso toDict fromDict <<>> anything
    toDict :: M.Map ByteString ByteString -> M.Map Text ByteString
    toDict = M.mapKeys T.decodeUtf8
    fromDict :: M.Map Text ByteString -> M.Map ByteString ByteString
    fromDict = M.mapKeys T.encodeUtf8

-- | Update value with partial data
update :: Monoid m => m -> Partial m -> m
update v (Partial x) = v `mappend` x

-- | Encode partial data
encodePartial :: (ModelSource s, Model m) => Partial m -> Either String s
encodePartial = encodeModel . partial

-- | Decode partial data
decodePartial :: (ModelSource s, Model m) => s -> Either String (Partial m)
decodePartial = fmap Partial . decodeModel
