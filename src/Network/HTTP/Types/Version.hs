{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Types.Version (
          Version (..)
        , major
        , minor) where

--------------------------------------------------------------------------------

--import qualified Data.ByteString.Short as Short
--import qualified Data.ByteString.Builder as Builder

--------------------------------------------------------------------------------

data Version =
        HTTP_0_9 |
        HTTP_1_0 |
        HTTP_1_1
        deriving (Eq, Show)

major :: Version -> Int
major HTTP_0_9 = 0
major HTTP_1_0 = 1
major HTTP_1_1 = 1

minor :: Version -> Int
minor HTTP_0_9 = 9
minor HTTP_1_0 = 0
minor HTTP_1_1 = 1
