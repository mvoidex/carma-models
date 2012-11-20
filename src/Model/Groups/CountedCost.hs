{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric #-}

module Model.Groups.CountedCost (
    CountedCost(..)
    ) where

import Data.Text (Text)

import Model.Base

import GHC.Generics

data CountedCost (k :: FieldKind) = CountedCost {
    counted :: Field k Int,
    serviceTarifOptions :: Field k Text }
        deriving (Generic)

instance Model CountedCost
