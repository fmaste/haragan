{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Response.Renderer.Binary (
        responseBuilder) where

--------------------------------------------------------------------------------

import Data.Time
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Builder as Builder
import qualified Network.HTTP.Response as R
import qualified Network.HTTP.Types.Version as Version
import qualified Network.HTTP.Types.Status as Status

--------------------------------------------------------------------------------

responseBuilder :: R.Response -> Builder.Builder
responseBuilder response =
        Builder.shortByteString "HTTP/" <>
        versionBuilder (R.version response) <>
        Builder.shortByteString " " <>
        statusBuilder (R.status response) <>
        Builder.shortByteString "\r\n" <>
        headersBuilder (R.headers response) <>
        Builder.shortByteString "\r\n" <>
        maybe mempty Builder.byteString (R.body response)

--------------------------------------------------------------------------------

versionBuilder :: Version.Version -> Builder.Builder
versionBuilder Version.HTTP_0_9 = Builder.shortByteString "0.9"
versionBuilder Version.HTTP_1_0 = Builder.shortByteString "1.0"
versionBuilder Version.HTTP_1_1 = Builder.shortByteString "1.1"

--------------------------------------------------------------------------------

statusBuilder :: Status.Status -> Builder.Builder
statusBuilder (Status.Status code bs) =
        Builder.string7 (show code) <>
        Builder.shortByteString " " <>
        Builder.byteString bs

--------------------------------------------------------------------------------

headersBuilder :: R.Headers -> Builder.Builder
headersBuilder headers =
        maybe mempty cacheControlBuilder (R.cacheControl headers)) <>
        maybe mempty connectionBuilder (R.connection headers)) <>
        maybe mempty contentEncodingBuilder (R.contentEncoding headers)) <>
        maybe mempty contentLanguageBuilder (R.contentLanguage headers)) <>
        maybe mempty contentLengthBuilder (R.contentLength headers) <>
        maybe mempty contentLocationBuilder (R.contentLocation headers) <>
        maybe mempty contentMD5Builder (R.contentMD5 headers) <>
        maybe mempty contentTypeBuilder (R.contentType headers) <>
        maybe mempty dateBuilder (R.date headers) <>
        maybe mempty expiresBuilder (R.expires headers) <>
        maybe mempty locationBuilder (R.location headers) <>
        maybe mempty pragmaBuilder (R.pragma headers) <>
        maybe mempty serverBuilder (R.server headers) <>
        mconcat setCookieBuilder (R.setCookie headers) <>
        maybe mempty transferEncodingBuilder (R.transferEncoding headers) <>
        maybe mempty xFrameOptionsBuilder (R.xFrameOptions headers)

headerBuilder :: Short.ShortByteString -> BS8.ByteString -> Builder.Builder
headerBuilder name value =
        Builder.shortByteString name <>
        Builder.shortByteString ": " <>
        Builder.byteString bs <>
        Builder.shortByteString "\r\n"

cacheControlBuilder :: BS8.ByteString -> Builder.Builder
cacheControlBuilder = headerBuilder "Cache-Control"

connectionBuilder :: BS8.ByteString -> Builder.Builder
connectionBuilder = headerBuilder "Connection"

contentEncodingBuilder :: BS8.ByteString -> Builder.Builder
contentEncodingBuilder = headerBuilder "Content-Encoding"

contentLanguageBuilder :: BS8.ByteString -> Builder.Builder
contentLanguageBuilder = headerBuilder "Content-Language"

contentLengthBuilder :: BS8.ByteString -> Builder.Builder
contentLengthBuilder = headerBuilder "Content-Length"

contentLocationBuilder :: BS8.ByteString -> Builder.Builder
contentLocationBuilder = headerBuilder "Content-Location"

contentMD5Builder :: BS8.ByteString -> Builder.Builder
contentMD5Builder = headerBuilder "Content-MD5"

contentTypeBuilder :: BS8.ByteString -> Builder.Builder
contentTypeBuilder = headerBuilder "Content-Type"

dateBuilder :: UTCTime -> Builder.Builder
dateBuilder t = headerBuilder
        "Date"
        (BS8.pack $ formatTime Time.defaultTimeLocale "%a, %d %b %0Y %T GMT" t)

expiresBuilder :: UTCTime -> Builder.Builder
expiresBuilder t = headerBuilder
        "Expires"
        (BS8.pack $ formatTime Time.defaultTimeLocale "%a, %d %b %0Y %T GMT" t)

locationBuilder :: BS8.ByteString -> Builder.Builder
locationBuilder = headerBuilder "Location"

pragmaBuilder :: BS8.ByteString -> Builder.Builder
pragmaBuilder = headerBuilder "Pragma"

serverBuilder :: BS8.ByteString -> Builder.Builder
serverBuilder = headerBuilder "Server"

setCookieBuilder :: BS8.ByteString -> Builder.Builder
setCookieBuilder = headerBuilder "Set-Cookie"

transferEncodingBuilder :: BS8.ByteString -> Builder.Builder
transferEncodingBuilder = headerBuilder "Transfer-Encoding"

xFrameOptionsBuilder :: BS8.ByteString -> Builder.Builder
xFrameOptionsBuilder = headerBuilder "X-Frame-Options"
