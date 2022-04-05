{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Database (

          Connection
        , connectRO, connectRW, close
        , Biz.withTransactionRO, Biz.withBizMethod

) where

--------------------------------------------------------------------------------

-- Package: base.
--
-- Home Package: haragan.
import qualified Database.Business as Biz
import qualified Database.Business.Method as BizMethod

--------------------------------------------------------------------------------

data Connection = Connection
        { _connectionStr :: String
        , _connectionBizVersionNumber :: Integer
        , _connectionBizConnection :: Biz.Connection
        }

connectRO :: String -> [Integer] -> IO Connection
connectRO connStr bizVersionNumber = do
        bizConn <- Biz.connect connStr bizVersionNumber False
        return (Connection connStr bizVersionNumber bizConn)

connectRW :: String -> [Integer] -> IO Connection
connectRW connStr bizVersionNumber = do
        bizConn <- Biz.connect connStr bizVersionNumber True
        return (Connection connStr bizVersionNumber bizConn)

close :: IO ()
close = return ()
