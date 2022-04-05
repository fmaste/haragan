{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Response (
          Response (Response)
        , version, status, headers, body
        , Headers (Headers)
        , emptyHeaders
        , cacheControl, connection, contentEncoding, contentLanguage, contentLength
        , contentLocation, contentMD5, contentType, date, expires, location, pragma 
        , server, setCookie, transferEncoding, xFrameOptions) where

--------------------------------------------------------------------------------

import qualified Data.Time as Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Types.Version as Version
import qualified Network.HTTP.Types.Status as Status

--------------------------------------------------------------------------------

data Response = Response {
          version :: Version.Version
        , status :: Status.Status
        , headers :: Headers
        , body :: Maybe BS.ByteString
} deriving (Eq, Show)

data Headers = Headers {
        -- private, max-age=0
          cacheControl :: Maybe BS8.ByteString
        -- close
        , connection :: Maybe BS8.ByteString
        -- gzip
        , contentEncoding :: Maybe BS8.ByteString
        , contentLanguage :: Maybe BS8.ByteString
        , contentLength :: Maybe BS8.ByteString
        , contentLocation :: Maybe BS8.ByteString
        , contentMD5 :: Maybe BS8.ByteString
        -- text/html; charset=UTF-8
        , contentType :: Maybe BS8.ByteString
        -- Fri, 09 Aug 2013 18:27:16 GMT
        , date :: Maybe Time.UTCTime
        -- HTTP/1.1 clients and caches MUST treat other invalid date formats,
        -- especially including the value "0", as in the past (i.e., "already
        -- expired").
        , expires :: Maybe Time.UTCTime
        , location :: Maybe BS8.ByteString
        , pragma :: Maybe BS8.ByteString
        , server :: Maybe BS8.ByteString
        , setCookie :: [BS8.ByteString]
        , transferEncoding :: Maybe BS8.ByteString
        , xFrameOptions :: Maybe BS8.ByteString
} deriving (Eq, Show)

emptyHeaders :: Headers
emptyHeaders = Headers {
          cacheControl = Nothing
        , connection = Nothing
        , contentEncoding = Nothing
        , contentLanguage = Nothing
        , contentLength = Nothing
        , contentLocation = Nothing
        , contentMD5 = Nothing
        , contentType = Nothing
        , date = Nothing
        , expires = Nothing
        , location = Nothing
        , pragma = Nothing
        , server = Nothing
        , setCookie = []
        , transferEncoding = Nothing
        , xFrameOptions = Nothing
}

--------------------------------------------------------------------------------
