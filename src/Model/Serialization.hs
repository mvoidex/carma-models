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

import Control.Arrow
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
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

dummy :: Selector c => f (Stor c a) -> Stor c a
dummy _ = undefined

instance (Selector c, DictionaryValue ByteString a) => GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (Stor c (OptField a)) where
    gser = fix $ \r -> try (entry_ (fromString $ storName (dummy r))) .:. Iso (opt Nothing Just . unStor) (Stor . maybe HasNo Has)

instance (Selector c, A.ToJSON a, A.FromJSON a) => GenericSerializable (Codec A.Object ToObject FromObject) (Stor c (OptField a)) where
    gser = fix $ \r -> try (member (fromString $ storName (dummy r)) value) .:. Iso (opt Nothing Just . unStor) (Stor . maybe HasNo Has)

instance (Selector c, Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) a) => GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

instance (Selector c, Serializable (Codec A.Object ToObject FromObject) a) => GenericSerializable (Codec A.Object ToObject FromObject) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

instance (Selector c, Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) a) => GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (Stor c (Group a)) where
    gser = fix $ \r -> withPrefix (storName $ dummy r) (ser .:. Iso (group . unStor) (Stor . Group)) where
        withPrefix :: String -> Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString) a -> Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString) a
        withPrefix pre (Codec (Encoding enc) (Decoding dec)) = Codec (Encoding enc') (Decoding dec') where
            enc' x = ToDictionary $ do
                dict <- lift $ execWriterT $ toDict $ enc x
                tell $ map (first $ C8.append preBS) dict
            dec' = do
                modify (M.mapKeys removePrefix)
                dec
                where
                    removePrefix k
                        | C8.isPrefixOf preBS k = C8.drop (C8.length preBS) k
                        | otherwise = k
            preBS = T.encodeUtf8 . T.pack $ pre ++ "_"

instance (Selector c, Serializable (Codec A.Object ToObject FromObject) a) => GenericSerializable (Codec A.Object ToObject FromObject) (Stor c (Group a)) where
    gser = fix $ \r -> withPrefix (storName $ dummy r) (ser .:. Iso (group . unStor) (Stor . Group)) where
        withPrefix :: String -> Codec A.Object ToObject FromObject a -> Codec A.Object ToObject FromObject a
        withPrefix pre (Codec (Encoding enc) (Decoding dec)) = Codec (Encoding enc') (Decoding dec') where
            enc' x = ToObject $ do
                pairs <- lift $ execWriterT $ runToObject $ enc x
                tell $ map (first $ T.append preT) pairs
            dec' = do
                modify (HM.fromList . map (first removePrefix) . HM.toList)
                dec
                where
                    removePrefix k
                        | T.isPrefixOf preT k = T.drop (T.length preT) k
                        | otherwise = k
            preT = T.pack $ pre ++ "_"

instance (Selector c, Serializable Pgser a) => GenericSerializable Pgser (Stor c (Group a)) where
    gser = fix $ \r -> withPrefix (storName $ dummy r) (ser .:. Iso (group . unStor) (Stor . Group)) where
        withPrefix :: String -> Pgser a -> Pgser a
        withPrefix pre (Pgser (Encoding enc) (Decoding dec) (Fields fs)) = Pgser (Encoding enc') (Decoding dec') (Fields fs') where
            enc' x = ToFields $ ToDictionary $ do
                dict <- lift $ execWriterT $ toDict $ toFields $ enc x
                tell $ map (first $ (pre_ ++)) dict
            dec' = FromFields $ do
                modify (M.mapKeys removePrefix)
                fromFields dec
                where
                    removePrefix k = fromMaybe k $ stripPrefix pre_ k
            fs' = map (first (pre_ ++)) fs
            pre_ = map toLower $ pre ++ "_"

instance (Selector c, Serializable Fields a) => GenericSerializable Fields (Stor c (Group a)) where
    gser = fix $ \r -> withPrefix (storName $ dummy r) (ser .:. Iso (group . unStor) (Stor . Group)) where
        withPrefix :: String -> Fields a -> Fields a
        withPrefix pre (Fields fs) = Fields $ map (first (pre_ ++)) fs where
            pre_ = pre ++ "_"

instance (Model m, GenIsoDerivable (GenericSerializable (Codec A.Object ToObject FromObject)) (m k)) => Serializable (Codec A.Object ToObject FromObject) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString))) (m k))
    => Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable Fields) (m k)) => Serializable Fields (m k)
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
