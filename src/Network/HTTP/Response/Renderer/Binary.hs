{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Response.Renderer.Binary (
        putResponse) where

--------------------------------------------------------------------------------

import Data.Time
import qualified Data.ByteString.Char8 as BS8
-- TODO: Use ByteString >= 0.10.2.0 Builder instead of the binary package.
import qualified Data.Binary.Put as Put
import qualified Network.HTTP.Response as R
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Version as Version

--------------------------------------------------------------------------------

-- I don't send the content lenght, I just close the connection.
putResponse :: R.Response -> Put.Put
putResponse response = do
        Put.putByteString "HTTP/"
        Put.putByteString (BS8.pack $ show $ Version.major $ R.version response)
        Put.putByteString "."
        Put.putByteString (BS8.pack $ show $ Version.minor $ R.version response)
        Put.putByteString " "
        Put.putByteString (BS8.pack $ show $ Status.code $ R.status response)
        Put.putByteString " "
        Put.putByteString (Status.message $ R.status response)
        Put.putByteString "\r\n"
        putHeaders (R.headers response)
        Put.putByteString "\r\n"
        maybe (return ()) Put.putByteString (R.body response)

--------------------------------------------------------------------------------

putHeaders :: R.Headers -> Put.Put
putHeaders headers = do
        maybe (return ()) putCacheControl (R.cacheControl headers)
        maybe (return ()) putConnection (R.connection headers)
        maybe (return ()) putContentEncoding (R.contentEncoding headers)
        maybe (return ()) putContentLanguage (R.contentLanguage headers)
        maybe (return ()) putContentLength (R.contentLength headers)
        maybe (return ()) putContentLocation (R.contentLocation headers)
        maybe (return ()) putContentMD5 (R.contentMD5 headers)
        maybe (return ()) putContentType (R.contentType headers)
        maybe (return ()) putDate (R.date headers)
        maybe (return ()) putExpires (R.expires headers)
        maybe (return ()) putLocation (R.location headers)
        maybe (return ()) putPragma (R.pragma headers)
        maybe (return ()) putServer (R.server headers)
        mapM_ putSetCookie (R.setCookie headers)
        maybe (return ()) putTransferEncoding (R.transferEncoding headers)
        maybe (return ()) putXFrameOptions (R.xFrameOptions headers)

putCacheControl :: BS8.ByteString -> Put.Put
putCacheControl bs = do
        Put.putByteString "Cache-Control: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putConnection :: BS8.ByteString -> Put.Put
putConnection bs = do
        Put.putByteString "Connection: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentEncoding :: BS8.ByteString -> Put.Put
putContentEncoding bs = do
        Put.putByteString "Content-Encoding: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentLanguage :: BS8.ByteString -> Put.Put
putContentLanguage bs = do
        Put.putByteString "Content-Language: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentLength :: BS8.ByteString -> Put.Put
putContentLength bs = do
        Put.putByteString "Content-Length: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentLocation :: BS8.ByteString -> Put.Put
putContentLocation bs = do
        Put.putByteString "Content-Location: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentMD5 :: BS8.ByteString -> Put.Put
putContentMD5 bs = do
        Put.putByteString "Content-MD5: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putContentType :: BS8.ByteString -> Put.Put
putContentType bs = do
        Put.putByteString "Content-Type: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putDate :: UTCTime -> Put.Put
putDate t = do
        Put.putByteString "Date: "
        Put.putByteString $ BS8.pack $ formatTime defaultTimeLocale "%a, %d %b %0Y %T GMT" t
        Put.putByteString "\r\n"

putExpires :: UTCTime -> Put.Put
putExpires t = do
        Put.putByteString "Expires: "
        Put.putByteString $ BS8.pack $ 
                formatTime defaultTimeLocale "%a, %d %b %0Y %T GMT" t
        Put.putByteString "\r\n"

putLocation :: BS8.ByteString -> Put.Put
putLocation bs = do
        Put.putByteString "Location: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putPragma :: BS8.ByteString -> Put.Put
putPragma bs = do
        Put.putByteString "Pragma: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putServer :: BS8.ByteString -> Put.Put
putServer bs = do
        Put.putByteString "Server: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putSetCookie :: BS8.ByteString -> Put.Put
putSetCookie bs = do
        Put.putByteString "Set-Cookie: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putTransferEncoding :: BS8.ByteString -> Put.Put
putTransferEncoding bs = do
        Put.putByteString "Transfer-Encoding: "
        Put.putByteString bs
        Put.putByteString "\r\n"

putXFrameOptions :: BS8.ByteString -> Put.Put
putXFrameOptions bs = do
        Put.putByteString "X-Frame-Options: "
        Put.putByteString bs
        Put.putByteString "\r\n"
