{-# LANGUAGE OverloadedStrings, DataKinds, ConstraintKinds, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, OverlappingInstances #-}

module Model.Serialization (
    -- * Model serialization
    JsonedModel, RedisedModel, PgsedModel,
    SerializedModel,
    modelJSON, modelRedis,
    encodeJSON, decodeJSON,
    encodeRedis, decodeRedis,

    -- * Helpers
    Textual, textual
    ) where

import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Function (fix)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString.Char8 as P

import GHC.Generics

import Data.Serialization
import Data.Serialization.Dictionary
import Data.Serialization.Text.Print
import Data.Serialization.Text.Attoparsec
import Data.Serialization.JSON.Aeson
import Data.Serialization.JSON.Aeson as S (toJSON, fromJSON)
import Data.Serialization.Postgresql

import Model.Base

instance (Selector c, DictionaryValue ByteString a) => GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (Stor c (OptField a)) where
    gser = fix $ \r -> try (entry_ (fromString $ storName (dummy r))) .:. Iso (opt Nothing Just . unStor) (Stor . maybe HasNo Has) where
        dummy :: f (Stor c a) -> Stor c a
        dummy _ = undefined

instance (Selector c, A.ToJSON a, A.FromJSON a) => GenericSerializable (Codec A.Object ToObject FromObject) (Stor c (OptField a)) where
    gser = fix $ \r -> try (member (fromString $ storName (dummy r)) value) .:. Iso (opt Nothing Just . unStor) (Stor . maybe HasNo Has) where
        dummy :: f (Stor c a) -> Stor c a
        dummy _ = undefined

instance (Selector c, Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) a) => GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

instance (Selector c, Serializable (Codec A.Object ToObject FromObject) a) => GenericSerializable (Codec A.Object ToObject FromObject) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

instance (Model m, GenIsoDerivable (GenericSerializable (Codec A.Object ToObject FromObject)) (m k)) => Serializable (Codec A.Object ToObject FromObject) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString))) (m k))
    => Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable Pgser) (m k)) => Serializable Pgser (m k)

type JsonedModel m k = (Model m, Serializable (Codec A.Object ToObject FromObject) (m k))
type RedisedModel m k = (Model m, Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (m k))
type PgsedModel m k = (Model m, Serializable Pgser (m k))

type SerializedModel m = (Model m, JsonedModel m Object, RedisedModel m Object, PgsedModel m Object, JsonedModel m Patch, RedisedModel m Patch, PgsedModel m Patch)

modelJSON :: (JsonedModel m k) => JsonMemberable (m k)
modelJSON = ser

modelRedis :: (RedisedModel m k) => Dictionarable ByteString ByteString (m k)
modelRedis = ser

-- | Encode model as JSON
encodeJSON :: (JsonedModel m k) => m k -> Either String ByteString
encodeJSON = encode (json <~> object modelJSON)

-- | Decode model from JSON
decodeJSON :: (JsonedModel m k) => ByteString -> Either String (m k)
decodeJSON = decode (json <~> object modelJSON)

-- | Encode model as Redis map
encodeRedis :: (RedisedModel m k) => m k -> Either String (M.Map ByteString ByteString)
encodeRedis = encode modelRedis

-- | Decode model from Redis map
decodeRedis :: (RedisedModel m k) => M.Map ByteString ByteString -> Either String (m k)
decodeRedis = decode modelRedis

-- | Implementations for encoding/decoding primitives info ByteString
type Textual a = CodecT ByteString Print Atto a

textual :: (Show a) => P.Parser a -> Textual a
textual p = codec printShow (atto p)

instance DictionaryValue ByteString Int where
    dictionaryValue = recode $ textual P.decimal

instance DictionaryValue ByteString Text where
    dictionaryValue = recode $ textual $ fmap T.decodeUtf8 P.takeByteString

instance DictionaryValue ByteString Double where
    dictionaryValue = recode $ textual P.double

instance DictionaryValue ByteString String where
    dictionaryValue = recode $ textual $ fmap (T.unpack . T.decodeUtf8) P.takeByteString

instance DictionaryValue ByteString Bool where
    dictionaryValue = Convertible fromBool toBool where
        fromBool True = Right "1"
        fromBool False = Right "0"
        toBool "1" = Right True
        toBool "0" = Right False
        toBool s = Left $ "Can't convert to bool: " ++ T.unpack (T.decodeUtf8 s)

instance A.ToJSON LocalTime where
    toJSON = A.toJSON . (floor :: POSIXTime -> Integer) . utcTimeToPOSIXSeconds . localTimeToUTC utc

instance A.FromJSON LocalTime where
    parseJSON = fmap (utcToLocalTime utc . posixSecondsToUTCTime . fromInteger) . A.parseJSON

instance DictionaryValue ByteString LocalTime where
    dictionaryValue = recode $ (textual P.decimal .:. Iso toPosix fromPosix) where
        toPosix = floor . utcTimeToPOSIXSeconds . localTimeToUTC utc
        fromPosix = utcToLocalTime utc . posixSecondsToUTCTime . fromInteger

instance DictionaryValue ByteString a => DictionaryValue ByteString (Maybe a) where
    dictionaryValue = Convertible to from where
        to Nothing = return ""
        to (Just v) = convertTo dictionaryValue v
        from "" = return Nothing
        from s = fmap Just $ convertFrom dictionaryValue s
