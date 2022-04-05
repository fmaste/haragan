{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Request (
        -- Request.
        -----------
          Request (Request)
        , method, uri, version, headers, body
        , Headers (Headers)
        , accept
        , acceptCharset, acceptEncoding, acceptLanguage
        , connection
        , contentLength, contentMD5, contentType
        , cookie
        , host
        , referer
        , userAgent
        , emptyHeaders
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Version as Version

-- The Request.
--------------------------------------------------------------------------------
-- http://www.w3.org/Protocols/

data Request = Request {
          method :: Method.Method
        -- Must always starts with '/'' and never end with '/'.
        , uri :: BS.ByteString
        , version :: Version.Version
        , headers :: Headers
        -- TODO: This one must be an IO (Maybe BS.ByteString)
        -- to allow to stream the body.
        , body :: Maybe BS.ByteString
} deriving (Eq, Show)

data Headers = Headers {
          accept :: Maybe BS.ByteString
        , acceptCharset :: Maybe BS.ByteString
        , acceptEncoding :: Maybe BS.ByteString
        , acceptLanguage :: Maybe BS.ByteString
        , connection :: Maybe BS.ByteString
        , contentLength :: Maybe BS.ByteString
        , contentMD5 :: Maybe BS.ByteString
        , contentType :: Maybe BS.ByteString
        , cookie :: Maybe BS.ByteString
        , host :: Maybe BS.ByteString
        , referer :: Maybe BS.ByteString
        , userAgent :: Maybe BS.ByteString
} deriving (Eq, Show)

emptyHeaders :: Headers
emptyHeaders = Headers {
          accept = Nothing
        , acceptCharset = Nothing
        , acceptEncoding = Nothing
        , acceptLanguage = Nothing
        , connection = Nothing
        , contentLength = Nothing
        , contentMD5 = Nothing
        , contentType = Nothing
        , cookie = Nothing
        , host = Nothing
        , referer = Nothing
        , userAgent = Nothing
}

-- HTTP 1.1 headers. (Plus cookies)
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
--------------------------------------------------------------------------------

-- Accept
-- Accept-Charset
-- Accept-Encoding
-- Accept-Language
-- Accept-Ranges
-- Age
-- Allow
-- Authorization
-- Cache-Control
-- Connection
-- Content-Encoding
-- Content-Language
-- Content-Length
-- Content-Location
-- Content-MD5
-- Content-Range
-- Content-Type
-- Cookie
-- Date
-- ETag
-- Expect
-- Expires
-- From
-- Host
-- If-Match
-- If-Modified-Since
-- If-None-Match
-- If-Range
-- If-Unmodified-Since
-- Last-Modified
-- Location
-- Max-Forwards
-- Pragma
-- Proxy-Authenticate
-- Proxy-Authorization
-- Range
-- Referer
-- Retry-After
-- Server
-- Set-Cookie
-- TE
-- Trailer
-- Transfer-Encoding
-- Upgrade
-- User-Agent
-- Vary
-- Via
-- Warning
-- WWW-Authenticate

-- Request only headers (plus cookies):
--------------------------------------------------------------------------------

-- Accept
-- Accept-Charset
-- Accept-Encoding
-- Accept-Language
-- Authorization
-- Cache-Control
-- Connection
-- Cookie
-- Content-Length
-- Content-MD5
-- Content-Type
-- Date
-- Expect
-- From
-- Host
-- If-Match
-- If-Modified-Since
-- If-None-Match
-- If-Range
-- If-Unmodified-Since
-- Max-Forwards
-- Pragma
-- Proxy-Authorization
-- Range
-- Referer
-- TE
-- Upgrade
-- User-Agent
-- Via
-- Warning

