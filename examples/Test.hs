{-# LANGUAGE OverloadedStrings, DataKinds, DeriveGeneric, MultiParamTypeClasses, ConstraintKinds, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

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
import Data.Time
import Data.Serialization
import Data.Serialization.Postgresql
import Database.PostgreSQL.Simple

import GHC.Generics

import Model

data My k = My {
    myInt :: Field k Int,
    myString :: Field k (Maybe String) }
        deriving (Generic)

data MyChild k = MyChild {
    myChild :: Field k String,
    myParent :: Parent (My k) }
        deriving (Generic)

instance Show (My Object) where
    show (My i s) = "My { myInt = " ++ show i ++ ", myString = " ++ show s ++ " }"

instance Show (My Patch) where
    show (My i s) = "My { myInt = " ++ opt "<null>" show i ++ ", myString = " ++ opt "<null>" show s ++ " }"

instance Show (MyChild Object) where
    show (MyChild n (Parent (My i s))) = "MyChild { myChild = " ++ show n ++ ", myInt = " ++ show i ++ ", myString = " ++ show s ++ " }"

instance Show (MyChild Patch) where
    show (MyChild n (Parent (My i s))) = "MyChild { myChild = " ++ opt "<null>" show n ++ ", myInt = " ++ opt "<null>" show i ++ ", myString = " ++ opt "<null>" show s ++ " }"

instance Model My where
    modelTable _ = "mytbl"

instance Model MyChild where
    modelTable _ = "mychildtbl"

test :: My Object
test = My 10 (Just "Hello!")

testp :: My Patch
testp = My HasNo (Has (Just "World!"))

testd :: My Meta
testd = desc

testcon :: ConnectInfo
testcon = defaultConnectInfo {
    connectUser = "carma_db_sync",
    connectPassword = "pass",
    connectDatabase = "carma" }

main :: IO ()
main = do
    putStrLn "=== JSON ==="
    either (const $ return ()) C8.putStrLn $ encodeJSON test
    -- {"myInt":10,"myString":"Hello!"}
    either (const $ return ()) C8.putStrLn $ encodeJSON testp
    -- {"myString":"World!"}
    either (const $ return ()) print $ encodeRedis test
    -- fromList [("myInt","10"),("myString","\"Hello!\"")]
    either (const $ return ()) print $ encodeRedis testp
    -- fromList [("myString","\"World!\"")]
    putStrLn "=== POSTGRESQL ==="
    con <- connect testcon
    execute_ con "drop table if exists mychildtbl"
    execute_ con "drop table if exists mytbl"
    create con (Table :: Table (My Object))
    create con (Table :: Table (MyChild Object))
    insert con (My 0 (Just "hello") :: My Object)
    insert con (MyChild "Some" (Parent (My 1 Nothing)) :: MyChild Object)
    update_ con (My HasNo (Has (Just "new")) :: My Patch) $ " where " ++ fieldName (myInt desc) ++ " = 0"
    putStrLn "=== SELECT ALL MY ==="
    v <- select_ con "" :: IO [My Object]
    mapM_ (either (const $ return ()) C8.putStrLn . encodeJSON) v
    --{"myInt":0,"myString":"new"}
    --{"myInt":1,"myString":null}
    putStrLn "=== SELECT ALL MYCHILD ==="
    v' <- select_ con "" :: IO [MyChild Object]
    mapM_ (either (const $ return ()) C8.putStrLn . encodeJSON) v'
    --{"myInt":1,"myString":null,"myChild":"Some"}
    return ()
