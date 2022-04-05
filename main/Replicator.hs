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

data Replicator = Replicator
        { replicatorConnStr :: String
        , replicatorLimit :: Int
        , replicatorDelay :: Int
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
                (connStr:limitStr:delayStr:_) -> do
                        let limit = ((read limitStr) :: Int)
                        let delay = ((read delayStr) :: Int)
                        return $ Replicator connStr limit delay
                _ -> error $ "<1>Usage: paramsFile or conn limit delay"
        conn <- DB.connectRW $ replicatorConnStr params
        forever $ do
                ans <- Biz.replicateBizMethods
                        conn
                        (replicatorLimit params)
                case ans of
                        -- Nothing done!
                        [] -> do
                                -- Wait 1 second (1000000 microseconds).
                                threadDelay $ 1000000 * (replicatorDelay params)
                        bizCallsIds -> do
                                -- A normal but significant condition.
                                putStrLn $ "<5>" ++ (show bizCallsIds)
