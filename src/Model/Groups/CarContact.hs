{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.CarContact (
    CarContact(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data CarContact (k :: FieldKind) = CarContact {
    name :: Field k Text,
    phone1 :: Field k Text,
    phone2 :: Field k Text,
    phone3 :: Field k Text,
    phone4 :: Field k Text,
    email :: Field k Text,
    contactOwner :: Field k Text,
    ownerName :: Field k Text,
    ownerPhone1 :: Field k Text,
    ownerPhone2 :: Field k Text,
    ownerPhone3 :: Field k Text,
    ownerPhone4 :: Field k Text,
    ownerEmail :: Field k Text }
        deriving (Generic)

instance Model CarContact
