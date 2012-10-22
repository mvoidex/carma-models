{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Model.Case (
    Case(..),
    caseIso
    ) where

import Data.Text (Text)

import Data.Iso
import Model.Base

-- | Case model
data Case = Case {
    caseId :: Field Int,
    caseCarMake :: Field Text,
    caseCarModel :: Field Text,
    caseCarProgram :: Field Text,
    caseCarVin :: Field Text,
    -- caseCarBuyDate :: Field UTCTime,
    caseCarPlateNum :: Field Text,
    caseCarCarModel :: Field Text,
    caseDiagnosis1 :: Field Text,
    caseDiagnosis2 :: Field Text,
    caseDealerCause :: Field Text,
    caseAddressAddress :: Field Text,
    -- caseCallDate :: Field UTCTime,
    caseCallTaker :: Field Text,
    caseContactName :: Field Text,
    caseComment :: Field Text,
    caseProgram :: Field Text,
    caseServices :: Field Text,
    caseContactOwnerName :: Field Text,
    casePartnerName :: Field Text }
        deriving (Eq, Ord, Read, Show)

-- | Make isomorphism Case <->tuple
$(makeIso "caseIso" ''Case)

-- | Define Monoid in terms of corresponding tuple
instance Monoid Case where
    mempty = memptyIso caseIso
    mappend = mappendIso caseIso

instance Model Case where
    asDict =
        field "id" (FieldInfo "Id of case") .**.
        field_ "car_make" .**.
        field_ "car_model"  .**.
        field_ "car_program"  .**.
        field_ "car_vin" .**.
        -- field_ "car_buyDate" .**.
        field_ "car_plateNum" .**.
        field_ "car_carModel" .**.
        field_ "diagnosis1" .**.
        field_ "diagnosis2" .**.
        field_ "dealerCause" .**.
        field_ "caseAddress_address" .**.
        -- field_ "callDate" .**.
        field_ "callTaker" .**.
        field_ "contact_name" .**.
        field_ "comment" .**.
        field_ "program" .**.
        field_ "services" .**.
        field_ "contact_ownerName" .**.
        field_ "partner_name"
        .:.
        caseIso
