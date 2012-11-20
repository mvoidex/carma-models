{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Contact (
    Contact(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data Contact (k :: FieldKind) = Contact {
    name :: Field k Text,
    phone1 :: Field k Text,
    phone2 :: Field k Text,
    phone3 :: Field k Text,
    phone4 :: Field k Text,
    email :: Field k Text }
        deriving (Generic)

instance Model Contact
