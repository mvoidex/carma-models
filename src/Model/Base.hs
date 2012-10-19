{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Model.Base (
    Field(..),
    emptyField, mkField,
    field,
    memptyIso, mappendIso,
    Model(..),
    Partial(..),
    update,
    encodeModel, decodeModel,
    encodePartial, decodePartial,

    module Data.Monoid,
    (.**.), (.:.)
    ) where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Text (Text)

import Data.Serialization
import Data.Serialization.JSON.Aeson
import Data.Serialization.JSON.Aeson as S (toJSON, fromJSON)
import qualified Data.Aeson as A

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

-- | Serialize\deserialize field
field :: (A.ToJSON a, A.FromJSON a) => Text -> Jsonable (Field a)
field name = (Iso Field fieldValue) <<>> try (member name)

-- | Monoid by iso
memptyIso :: (Monoid b) => Iso a b -> a
memptyIso i = comorph i mempty

mappendIso :: (Monoid b) => Iso a b -> a -> a -> a
mappendIso i l r = comorph i $ mappend (morph i l) (morph i r)

class Monoid m => Model m where
    asJson :: Jsonable m

instance Model m => A.ToJSON m where
    toJSON = S.toJSON asJson

instance Model m => A.FromJSON m where
    parseJSON = S.fromJSON asJson

-- | Dummy type to show, that model is not value itself, it's just partial value
newtype Partial m = Partial { partial :: m }
    deriving (Eq, Ord, Read, Show)

instance Monoid m => Monoid (Partial m) where
    mempty = Partial mempty
    mappend (Partial l) (Partial r) = Partial $ mappend l r

-- | Update value with partial data
update :: Monoid m => m -> Partial m -> m
update v (Partial x) = v `mappend` x

-- | Decode model from JSON
decodeModel :: Model m => ByteString -> Either String m
decodeModel = decode (json <~> asJson)

-- | Encode model to JSON
encodeModel :: Model m => m -> Either String ByteString
encodeModel = encode (json <~> asJson)

-- | Decode partial data
decodePartial :: Model m => ByteString -> Either String (Partial m)
decodePartial = fmap Partial . decode (json <~> asJson)

-- | Encode partial data
encodePartial :: Model m => Partial m -> Either String ByteString
encodePartial = encode (json <~> asJson) . partial
