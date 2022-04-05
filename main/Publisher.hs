{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- Package: base.
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (stdout, stderr, BufferMode (..), hSetBuffering)
-- Package: haragan.
import qualified Database as DB
import qualified Database.Business as Biz

--------------------------------------------------------------------------------

data Publisher = Publisher
        { publisherPubConnStr :: String
        , publisherDstConnStr :: String
        , publisherLimit :: Int
        , publisherDelay :: Int
        } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

main :: IO ()
main = do
        -- Systemd journald uses strange buffering when piping the output of the
        -- binary of a service unit. Force it to normal mode, as Upstart did.
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
        args <- getArgs
        params <- case args of
                (paramsFile:[]) -> do
                        paramsStr <- readFile paramsFile
                        return (read paramsStr)
                (pubConnStr:dstConnStr:limitStr:delayStr:_) -> do
                        let limit = ((read limitStr) :: Int)
                        let delay = ((read delayStr) :: Int)
                        return $ Publisher pubConnStr dstConnStr limit delay
                _ -> error $ "<1>Usage: paramsFile or connPub conDst limit delay"
        pubConn <- DB.connectRW $ publisherPubConnStr params
        dstConn <- DB.connectRW $ publisherDstConnStr params
        -- We assume the final destination shard is always the same as the one
        -- of the database we are connecting.
        -- To do a multihop or many to many distribution system we need to
        -- provide manually the destination shard ID.
        let dstShardId = Biz.getShardId dstConn
        forever $ do
                ans <- Biz.publishBizMethods
                        pubConn
                        dstConn
                        dstShardId
                        (publisherLimit params)
                case ans of
                        -- Nothing done!
                        [] -> do
                                -- Wait (1 second = 1000000 microseconds).
                                threadDelay $ 1000000 * (publisherDelay params)
                        pubsIds -> do
                                -- A normal but significant condition.
                                putStrLn $ "<5>" ++ (show pubsIds)
