{-# LANGUAGE OverloadedStrings, DataKinds, DeriveGeneric #-}

module Main (
    test, testp, testd,
    main
    ) where

import Control.Monad.IO.Class

import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Redis as R

import GHC.Generics

import Model

data My k = My {
    myInt :: Field k Int,
    myString :: Field k String }
        deriving (Generic)

instance Model My

test :: My Object
test = My 10 "Hello!"

testp :: My Patch
testp = My Nothing (Just "World!")

testd :: My Meta
testd = desc

main :: IO ()
main = do
    either (const $ return ()) C8.putStrLn $ encodeJSON test
    -- {"myInt":10,"myString":"Hello!"}
    either (const $ return ()) C8.putStrLn $ encodeJSON testp
    -- {"myInt":null,"myString":"World!"}
    either (const $ return ()) print $ encodeRedis test
    -- fromList [("myInt","10"),("myString","\"Hello!\"")]
