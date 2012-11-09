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
    caseId :: Field k Int,
    caseCarMake :: Field k Text,
    caseCarModel :: Field k Text,
    caseCarProgram :: Field k Text,
    caseCarVin :: Field k Text,
    caseCarBuyDate :: Field k UTCTime,
    caseCarPlateNum :: Field k Text,
    caseCarCarModel :: Field k Text,
    caseDiagnosis1 :: Field k Text,
    caseDiagnosis2 :: Field k Text,
    caseDealerCause :: Field k Text,
    caseAddressAddress :: Field k Text,
    caseCallDate :: Field k UTCTime,
    caseCallTaker :: Field k Text,
    caseContactName :: Field k Text,
    caseComment :: Field k Text,
    caseProgram :: Field k Text,
    caseServices :: Field k Text,
    caseContactOwnerName :: Field k Text,
    casePartnerName :: Field k Text }
        deriving (Generic)

instance Model Case where
    desc = Case
        (FieldMeta "id")
        (FieldMeta "car_make")
        (FieldMeta "car_model")
        (FieldMeta "car_program")
        (FieldMeta "car_vin")
        (FieldMeta "car_buyDate")
        (FieldMeta "car_plateNum")
        (FieldMeta "car_carModel")
        (FieldMeta "diagnosis1")
        (FieldMeta "diagnosis2")
        (FieldMeta "dealerCause")
        (FieldMeta "caseAddress_address")
        (FieldMeta "callDate")
        (FieldMeta "callTaker")
        (FieldMeta "contact_name")
        (FieldMeta "comment")
        (FieldMeta "program")
        (FieldMeta "services")
        (FieldMeta "contact_ownerName")
        (FieldMeta "partner_name")
    modelTable _ = "casetbl"

