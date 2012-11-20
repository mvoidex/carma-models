{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.VinContact (
    VinContact(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data VinContact (k :: FieldKind) = VinContact {
    name :: Field k Text,
    phone1 :: Field k Text,
    email :: Field k Text }
        deriving (Generic)

instance Model VinContact
