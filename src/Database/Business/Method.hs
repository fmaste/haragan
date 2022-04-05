{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Database.Business.Method (
) where

--------------------------------------------------------------------------------

data BizMethod = BizMethod
        { _connectionStr :: String
        , _connectionServerId :: Integer
        , _connectionBizVersionId :: Integer
        , _connectionWriteAccess :: Bool
        , _connectionSemaphor :: MVar.MVar ()
        , _connectionInternal :: Internal.Connection
        }
