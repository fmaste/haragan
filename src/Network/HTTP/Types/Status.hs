{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Types.Status (
          Status (..)
        , code
        , message) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

data Status = Status Int BS.ByteString
        deriving (Eq, Show)

code :: Status -> Int
code (Status int _) = int

message :: Status -> BS.ByteString
message (Status _ msg) = msg
