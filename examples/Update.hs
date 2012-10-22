{-# LANGUAGE OverloadedStrings #-}

module Main (
    test, run, redisTest,
    main
    ) where

import Control.Monad.IO.Class

import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Redis as R

import Model
import Model.Case

test :: Case
test = mempty

-- > {}
-- {"name":"Hello"}
-- > {"name":"Hello"}
-- {"second":12.3}
-- > {"second":12.3,"name":"Hello"}
-- {"second":122.2}
-- > {"second":122.2,"name":"Hello"}
-- {"first":12, "name":"World"}
-- > {"second":122.2,"name":"World","first":12}
run :: IO ()
run = do
    printDecl
    run' test
    where
        printDecl :: IO ()
        printDecl = mapM_ printOne $ modelInfo test where
            printOne (k, Nothing) = T.putStrLn k
            printOne (k, Just (FieldInfo desc)) = T.putStrLn $ T.concat [k, ": ", desc]
        run' :: Case -> IO ()
        run' v = do
            putStr "> "
            either putStrLn C8.putStrLn $ encodeModel v
            i <- getLine
            either onError onUpdate $ decodePartial (C8.pack i)
            where
                onError e = putStrLn e >> run' v
                onUpdate x = run' $ update v x

redisTest :: String -> IO ()
redisTest k = do
    con <- R.connect R.defaultConnectInfo
    R.runRedis con $ do
        (Right m) <- R.hgetall (fromString k)
        liftIO $ either putStrLn printCase $ decodeModel (M.fromList m)
    return ()
    where
        printCase :: Case -> IO ()
        printCase = print

main :: IO ()
main = do
    s <- getLine
    case s of
        "run" -> run
        k -> redisTest k
