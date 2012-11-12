{-# LANGUAGE OverloadedStrings, DataKinds, DeriveGeneric, MultiParamTypeClasses, ConstraintKinds, FlexibleContexts, UndecidableInstances #-}

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
import Data.Serialization
import Data.Serialization.Postgresql
import Database.PostgreSQL.Simple

import GHC.Generics

import Model

data My k = My {
    myInt :: Field k Int,
    myString :: Field k String }
        deriving (Generic)

instance Model My where
    modelTable _ = "mytbl"

test :: My Object
test = My 10 "Hello!"

testp :: My Patch
testp = My HasNo (Has "World!")

testd :: My Meta
testd = desc

testcon :: ConnectInfo
testcon = defaultConnectInfo {
    connectUser = "carma_db_sync",
    connectPassword = "pass",
    connectDatabase = "carma" }

main :: IO ()
main = do
    either (const $ return ()) C8.putStrLn $ encodeJSON test
    -- {"myInt":10,"myString":"Hello!"}
    either (const $ return ()) C8.putStrLn $ encodeJSON testp
    -- {"myString":"World!"}
    either (const $ return ()) print $ encodeRedis test
    -- fromList [("myInt","10"),("myString","\"Hello!\"")]
    either (const $ return ()) print $ encodeRedis testp
    -- fromList [("myString","\"World!\"")]
    con <- connect testcon
    execute_ con "drop table mytbl"
    create con (Table :: Table (My Object))
    insert con (My 0 "hello" :: My Object)
    update_ con (My HasNo (Has "new") :: My Patch) " where myint = 0"
    v <- select_ con "" :: IO [My Object]
    mapM_ (either (const $ return ()) C8.putStrLn . encodeJSON) v
    -- {"myInt":0,"myString":"new"}
    return ()
