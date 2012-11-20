{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Address (
    Address(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data Address (k :: FieldKind) = Address {
    address :: Field k Text,
    comment :: Field k Text,
    coords :: Field k Text,
    map :: Field k Text }
        deriving (Generic)

instance Model Address
