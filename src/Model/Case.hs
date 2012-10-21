{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Model.Case (
    Case(..),
    caseIso
    ) where

import Data.Iso
import Model.Base

-- | Case model
data Case = Case {
    caseInt :: Field Int,
    caseDouble :: Field Double,
    caseString :: Field String }
        deriving (Eq, Ord, Read, Show)

-- | Make isomorphism Case <->tuple
$(makeIso "caseIso" ''Case)

-- | Define Monoid in terms of corresponding tuple
instance Monoid Case where
    mempty = memptyIso caseIso
    mappend = mappendIso caseIso

instance Model Case where
    asDict =
        field "first" (FieldInfo "First field") .**.
        field "second" (FieldInfo "Second field") .**.
        field "name" (FieldInfo "Third field")
        .:.
        caseIso
