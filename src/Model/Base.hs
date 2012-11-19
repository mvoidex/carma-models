{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds, UndecidableInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Base (
    -- * Model field
    FieldKind(..),
    FieldMeta(..),
    Field,
    Model(..),
    OptField(..), opt,
    Parent(..),

    patch, union
    ) where

import Data.Function (fix)

import GHC.Generics

import Data.Serialization
import Data.Serialization.Postgresql

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

instance (Selector c, Serializable Desc a) => GenericSerializable Desc (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

-- | 'Model' class, defines way to serialize data
class (Generic (m Object), Generic (m Patch), Generic (m Meta)) => Model m where
    desc :: m Meta
    modelTable :: Table (m k) -> String

    default desc :: Serializable Desc (m Meta) => m Meta
    desc = getDesc ser

instance (Model m, GenIsoDerivable (GenericSerializable Desc) (m Meta)) => Serializable Desc (m Meta)

instance Model m => InTable (m Object) where
    table = modelTable
instance Model m => InTable (m Patch) where
    table = modelTable

patch :: (Model m, GenericPatch (m Object) (m Patch)) => m Object -> m Patch -> m Object
patch = genPatch

union :: (Model m, GenericUnion (m Patch)) => m Patch -> m Patch -> m Patch
union = genUnion

class GenericPatch a b where
    genPatch :: a -> b -> a

instance (Model m, GenIso (Rep (m Object)), GenIso (Rep (m Patch)), GenericPatch (IsoRep (m Object)) (IsoRep (m Patch))) => GenericPatch (m Object) (m Patch) where
    genPatch x y = comorph giso $ genPatch (morph giso x) (morph giso y)

instance GenericPatch a (OptField a) where
    genPatch _ (Has x) = x
    genPatch x HasNo = x

instance (GenericPatch a c, GenericPatch b d) => GenericPatch (a, b) (c, d) where
    genPatch (x, y) (z, t) = (genPatch x z, genPatch y t)

instance (GenericPatch a c, GenericPatch b d) => GenericPatch (Either a b) (Either c d) where
    genPatch (Left x) (Left z) = Left (genPatch x z)
    genPatch (Left x) (Right _) = Left x
    genPatch (Right y) (Left _) = Right y
    genPatch (Right y) (Right t) = Right (genPatch y t)

instance (GenericPatch a b, Selector sa, Selector sb) => GenericPatch (Stor sa a) (Stor sb b) where
    genPatch (Stor x) (Stor y) = Stor (genPatch x y)

instance (GenericPatch a b, Constructor ca, Constructor cb) => GenericPatch (Ctor ca a) (Ctor cb b) where
    genPatch (Ctor x) (Ctor y) = Ctor (genPatch x y)

instance (GenericPatch a b, Datatype da, Datatype db) => GenericPatch (Data da a) (Data db b) where
    genPatch (Data x) (Data y) = Data (genPatch x y)

class GenericUnion a where
    genUnion :: a -> a -> a

instance (Model m, GenIsoDerivable GenericUnion (m Patch)) => GenericUnion (m Patch) where
    genUnion x y = comorph giso $ genUnion (morph giso x) (morph giso y)

instance GenericUnion (OptField a) where
    genUnion x HasNo = x
    genUnion _ (Has y) = (Has y)

instance (GenericUnion a, GenericUnion b) => GenericUnion (a, b) where
    genUnion (lx, ly) (rx, ry) = (genUnion lx rx, genUnion ly ry)

instance (GenericUnion a, GenericUnion b) => GenericUnion (Either a b) where
    genUnion (Left x) (Left y) = Left (genUnion x y)
    genUnion (Left x) (Right _) = Left x
    genUnion (Right x) (Left _) = Right x
    genUnion (Right x) (Right y) = Right (genUnion x y)

instance (GenericUnion a, Selector s) => GenericUnion (Stor s a) where
    genUnion (Stor x) (Stor y) = Stor (genUnion x y)

instance (GenericUnion a, Constructor c) => GenericUnion (Ctor c a) where
    genUnion (Ctor x) (Ctor y) = Ctor (genUnion x y)

instance (GenericUnion a, Datatype d) => GenericUnion (Data d a) where
    genUnion (Data x) (Data y) = Data (genUnion x y)
