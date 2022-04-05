{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Types.Method (
        Method (..),
        builder) where

--------------------------------------------------------------------------------

--import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Builder as Builder

--------------------------------------------------------------------------------

data Method =
        GET |
        POST |
        HEAD |
        PUT |
        DELETE |
        TRACE |
        CONNECT |
        OPTIONS |
        PATCH
        deriving (Eq, Show)

builder :: Method -> Builder.Builder
builder GET = Builder.shortByteString "GET"
builder POST = Builder.shortByteString "POST"
builder HEAD = Builder.shortByteString "HEAD"
builder PUT = Builder.shortByteString "PUT"
builder DELETE = Builder.shortByteString "DELETE"
builder TRACE = Builder.shortByteString "TRACE"
builder CONNECT = Builder.shortByteString "CONNECT"
builder OPTIONS = Builder.shortByteString "OPTIONS"
builder PATCH = Builder.shortByteString "PATCH"

