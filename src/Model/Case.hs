{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module Model.Case (
    Case(..)
    ) where

import GHC.Generics

import Data.Text (Text)
import Data.Time

import Model.Base

-- | Case model
data Case (k :: FieldKind) = Case {
    id :: Field k Int,
    car_make :: Field k Text,
    car_model :: Field k Text,
    car_program :: Field k Text,
    car_vin :: Field k Text,
    car_buyDate :: Field k LocalTime,
    car_plateNum :: Field k Text,
    diagnosis1 :: Field k Text,
    diagnosis2 :: Field k Text,
    dealerCause :: Field k Text,
    caseAddress_address :: Field k Text,
    callDate :: Field k LocalTime,
    callTaker :: Field k Text,
    comment :: Field k Text,
    program :: Field k Text,
    services :: Field k Text }
        deriving (Generic)

instance Model Case where
    modelTable _ = "casetbl"
