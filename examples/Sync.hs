{-# LANGUAGE OverloadedStrings, DataKinds, ConstraintKinds, FlexibleContexts #-}

module Main (
    main
    ) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import Data.String
import Data.Function (fix)

import qualified Database.Redis as R
import qualified Database.PostgreSQL.Simple as P

import Data.Serialization
import Data.Serialization.Postgresql

import System.Environment

import Model
import Model.Case (Case)
import qualified Model.Case as Case
import Model.Partner (Partner)
import qualified Model.Partner as Partner

psqlCon :: P.ConnectInfo
psqlCon = P.defaultConnectInfo {
    P.connectUser = "carma_db_sync",
    P.connectPassword = "pass",
    P.connectDatabase = "carma" }

sync :: (ModelTable m, PgsedModel m Patch, RedisedModel m Patch) => R.Connection -> P.Connection -> String -> String -> (Int -> m Patch -> m Patch) -> Int  -> IO (Maybe (m Patch))
sync rcon pcon mdlName tblName addId maxId = fix $ \r -> do
    createIfNot pcon $ getTable r
    forM [1..maxId] $ \i -> do
        putStr $ mdlName ++ ":" ++ show i ++ "... "
        (Right val) <- R.runRedis rcon $ R.hgetall $ fromString $ mdlName ++ ":" ++ show i
        if null val
            then putStrLn "no data"
            else do
                case decodeRedis $ M.fromList val of
                    Left err -> putStrLn $ "Error on " ++ mdlName ++ ":" ++ show i ++ ": " ++ err
                    Right val' -> do
                        let
                            sameType :: m Patch -> IO (Maybe (m Patch)) -> IO ()
                            sameType _ _ = return ()
                        sameType val' r
                        [(P.Only ex)] <- P.query pcon (fromString $ "select count(*) > 0 from " ++ tblName ++ " where id = ?") (P.Only i)
                        if ex
                            then do
                                putStrLn "updating..."
                                void $ update pcon val' " where id = ?" (P.Only i)
                            else do
                                putStrLn "inserting..."
                                void $ insert pcon (addId i val')
    return Nothing
    where
        getTable :: ModelTable m => IO (Maybe (m Patch)) -> Table (m Patch)
        getTable _ = Table

main :: IO ()
main = do
    args@(~([mdl])) <- getArgs
    if length args /= 1
        then putStrLn "Usage: sync <model>"
        else do
            rcon <- R.connect R.defaultConnectInfo
            pcon <- P.connect psqlCon
            Right (Just cnt) <- R.runRedis rcon $ R.get $ fromString $ "global:" ++ mdl ++ ":id"
            maxId <- maybe (error "Invalid max id") (return . fst) $ C8.readInt cnt
            putStrLn $ "Max id: " ++ show cnt
            case mdl of
                "case" -> void $ (sync rcon pcon "case" (modelTable (Table :: Table (Case Patch))) (\i v -> v { Case.id = Has i }) maxId :: IO (Maybe (Case Patch)))
                "partner" -> void $ (sync rcon pcon "partner" (modelTable (Table :: Table (Partner Patch))) (\i v -> v { Partner.id = Has i }) maxId :: IO (Maybe (Partner Patch)))
