{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeOperators, DataKinds, PolyKinds, TypeFamilies, RankNTypes, ConstraintKinds, DefaultSignatures, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Base (
    -- * Model field
    FieldKind(..),
    FieldMeta(..),
    Field,
    Model(..),
    OptField(..), opt,
    
    -- * Model serialization
    modelJSON, modelRedis,
    encodeJSON, decodeJSON,
    encodeRedis, decodeRedis,

    -- * Helpers
    Textual, textual,

    module Data.Monoid
    ) where

import qualified Data.Map as M
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Function (fix)

import GHC.Generics
import GHC.TypeLits

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

 -- | Field kind
data FieldKind = Object | Patch | Meta
    deriving (Eq, Ord, Read, Show)

-- | Field meta info
data FieldMeta = FieldMeta { fieldName :: String } deriving (Eq, Ord, Read, Show)

-- | Model field
type family Field (k :: FieldKind) a :: *

type instance Field Object a = a
type instance Field Patch a = OptField a
type instance Field Meta a = FieldMeta

-- | Desctiption collector
newtype Desc a = Desc { getDesc :: a } deriving (Eq, Ord, Read, Show)

instance Combine Desc where
    (Desc l) .*. (Desc r) = Desc (l, r)
    (Desc l) .+. (Desc r) = Desc (Left l)
    (Desc s) .:. iso = Desc (comorph iso s)

instance GenericCombine Desc

instance Selector c => GenericSerializable Desc (Stor c FieldMeta) where
    gser = fix $ Desc . Stor . FieldMeta . storName . dummy where
        dummy :: Desc (Stor c FieldMeta) -> Stor c FieldMeta
        dummy _ = undefined

-- | 'Model' class, defines way to serialize data
class (Generic (m Object), Generic (m Patch), Generic (m Meta)) => Model m where
    desc :: m Meta
    modelTable :: Table (m k) -> String

    default desc :: Serializable Desc (m Meta) => m Meta
    desc = getDesc ser

instance (A.FromJSON a, A.ToJSON a) => Serializable (Codec A.Object ToObject FromObject) (OptField a) where
    ser = member "" (try value .:. Iso (opt Nothing Just) (maybe HasNo Has))

instance (Model m, GenIsoDerivable (GenericSerializable (Codec A.Object ToObject FromObject)) (m k)) => Serializable (Codec A.Object ToObject FromObject) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString))) (m k))
    => Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (m k)
instance (Model m, GenIsoDerivable (GenericSerializable Pgser) (m k)) => Serializable Pgser (m k)
instance (Model m, GenIsoDerivable (GenericSerializable Desc) (m Meta)) => Serializable Desc (m Meta)

instance Model m => InTable (m Object) where
    table = modelTable
instance Model m => InTable (m Patch) where
    table = modelTable

type JsonedModel m k = (Model m, Serializable (Codec A.Object ToObject FromObject) (m k))
type RedisedModel m k = (Model m, Serializable (Codec (M.Map ByteString ByteString) (ToDictionary ByteString ByteString) (FromDictionary ByteString ByteString)) (m k))

modelJSON :: (JsonedModel m k) => JsonMemberable (m k)
modelJSON = ser

modelRedis :: (RedisedModel m k) => Dictionarable ByteString ByteString (m k)
modelRedis = ser

-- | Encode model as JSON
encodeJSON :: (JsonedModel m k) => m k -> Either String ByteString
encodeJSON = encode (json <~> object modelJSON)

-- | Decode model from JSON
decodeJSON :: (JsonedModel m k) => ByteString -> Either String (m k)
decodeJSON = undefined

-- | Encode model as Redis map
encodeRedis :: (RedisedModel m k) => m k -> Either String (M.Map ByteString ByteString)
encodeRedis = encode modelRedis

-- | Decode model from Redis map
decodeRedis :: (RedisedModel m k) => M.Map ByteString ByteString -> Either String (m k)
decodeRedis = decode modelRedis

--Data.Serialization.encode (ser :: Codec (Data.Aeson.Object) ToObject FromObject (My Model.Base.Object)) test
-- Right fromList [("myInt",Number 10),("myString",String "Hello!")]

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
