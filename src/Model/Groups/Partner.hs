{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.Partner (
    Partner(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data Partner (k :: FieldKind) = Partner {
    partner :: Field k Text,
    partnerId :: Field k Int,
    address :: Field k Text,
    partnerMap :: Field k Text,
    partnerTable :: Field k Text }
        deriving (Generic)

instance Model Partner
