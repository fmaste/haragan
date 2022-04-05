{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Request.Parser.Binary (
        -- Parser.
        ----------
        getRequest
) where

--------------------------------------------------------------------------------

import Data.Maybe
import Control.Monad
import Text.Read (readMaybe)
-- Package: bytestring.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Binary.Get as Get
-- Package: introspection.
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Version as Version
import Network.HTTP.Request

-- The parser.
--------------------------------------------------------------------------------

-- This very simple parser takes advantage of the Get monad error handler to
-- avoid adding an extra layer of complexity. This way I can amortize the 
-- overhead (performance cost) of using a Monad for parsing.
-- So, while parsing inside the Get monad and an early error is encoutered 
-- "error" is called, this error shoould be catched with the Decoder.
getRequest :: Get.Get Request
getRequest = do
        -- In the interest of robustness, servers SHOULD ignore any empty line(s)
        -- received where a Request-Line is expected. In other words, if the server
        -- is reading the protocol stream at the beginning of a message and 
        -- receives a CRLF first, it should ignore the CRLF.
        -- Parse this CRLF that may appear at the beginning?
        -- For performance reasons, no!!, just fail!!!
        parsedMethod <- getMethod
        Get.skip 1 -- The space.
        -- A server SHOULD return 414 (Request-URI Too Long) status if a URI is 
        -- longer than the server can handle (see section 10.4.15).
        -- Note: Servers ought to be cautious about depending on URI lengths above
        -- 255 bytes, because some older client or proxy implementations might not 
        -- properly support these lengths.
        -- This rare condition is only likely to occur when a client has improperly
        -- converted a POST request to a GET request with long query information, 
        -- when the client has descended into a URI "black hole" of redirection 
        -- (e.g., a redirected URI prefix that points to a suffix of itself), or 
        -- when the server is under attack by a client attempting to exploit 
        -- security holes present in some servers using fixed-length buffers for 
        -- reading or manipulating the Request-URI.
        uriLength <- Get.lookAhead $ getUntil 0 ' ' 255
        parsedUri <- case uriLength of
                Nothing -> error "URI larger than 255 bytes found."
                (Just len) -> if len == 1
                        then do
                                Get.skip 1
                                return "/"
                        else do
                                prefix <- Get.getByteString (len - 1)
                                suffix <- Get.getByteString 1
                                if suffix == "/"
                                        then return prefix
                                        else return $ BS8.append prefix suffix
        Get.skip 1 -- The space.
        parsedVersion <- getVersion
        Get.skip 2 -- The \r\n
        parsedHeaders <- getHeaders emptyHeaders
        Get.skip 2 -- The \r\n
        maybeBody <- case (contentLength parsedHeaders) of
                Nothing -> return Nothing
                Just contentLengthBs -> do
                        let contentLengthStr = BS8.unpack contentLengthBs
                        let maybeContentLengthInt = readMaybe contentLengthStr :: Maybe Int
                        case maybeContentLengthInt of
                                Nothing -> error "Invalid Content-Length."
                                Just contentLengthInt ->if contentLengthInt == 0
                                        then return Nothing
                                        else do
                                                bodyBs <- getBody contentLengthInt
                                                return (Just bodyBs)
        return $ Request parsedMethod parsedUri parsedVersion parsedHeaders maybeBody

getUntil :: Int -> Char -> Int -> Get.Get (Maybe Int)
getUntil start stop limit = do
        if start > (limit + 1)
                then return Nothing
                else do
                        c <- Get.getByteString 1
                        if (BS8.head c) == stop
                                then return (Just start)
                                else getUntil (start + 1) stop limit

-- HTTP standard method (as defined by RFC 2616, and PATCH which is defined
-- by RFC 5789) (OPTIONS, GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, PATCH).
getMethod :: Get.Get Method.Method
getMethod = do
        -- There is no need to look ahead here, methods are well-known.
        methodPart <- Get.getByteString 2
        -- Ordered by use frequency (assumed by me).
        case methodPart of
                "GE" -> do
                        Get.skip 1
                        return Method.GET
                "PO" -> do
                        Get.skip 2
                        return Method.POST
                "HE" -> do
                        Get.skip 2
                        return Method.HEAD
                "PU" -> do
                        Get.skip 1
                        return Method.PUT
                "DE" -> do
                        Get.skip 4
                        return Method.DELETE
                "TR" -> do
                        Get.skip 3
                        return Method.TRACE
                "CO" -> do
                        Get.skip 5
                        return Method.CONNECT
                "OP" -> do
                        Get.skip 5
                        return Method.OPTIONS
                "PA" -> do
                        Get.skip 3
                        return Method.PATCH
                _ -> do
                        -- When method can't be parsed my assumuption is complete failure.
                        -- Maybe trailing \r\n where sent or not parsed, but I don't care.
                        -- The correct thing is sending a method not supported header.
                        -- I need speed!!!!!
                        error "Unknown HTTP method found."

-- TODO: HTTP 2
getVersion :: Get.Get Version.Version
getVersion = do
        Get.skip 7 -- "HTTP/?."
        versionBS <- Get.getByteString 1
        -- Ordered by use frequency (assumed by me).
        return $ case versionBS of 
                "1" -> Version.HTTP_1_1
                "0" -> Version.HTTP_1_0
                -- HTTP 0.9 is too old, don't use it.
                -- But fail somewhere else with the correct HTTP response.
                -- This is a rare case, so it's not worth failing earlier to speed up.
                "9" -> Version.HTTP_0_9
                -- If HTTP version is not one of these three I fail earlier.
                -- I'm assuming that a bad request was sent when this happens.
                -- The correct thing to do is to send the HTTP version not supported.
                _ -> error "Unknown HTTP version found."

getHeaders :: Headers -> Get.Get Headers
getHeaders h = do
        peek <- Get.lookAhead $ Get.getByteString 2
        case peek of
                "\r\n" -> return h
                _ -> do
                        f <- getHeaderFromName peek
                        -- I use as limit the common cookie restrictions.
                        -- The size of all cookies is 4K, common browsers truncate here.
                        -- Some browsers have a cookies limit per domain (Firefox has 50).
                        l <- Get.lookAhead $ getUntil 0 '\r' 4096
                        -- Fail with a bad request is a large header is found.
                        -- TODO: Search the protocol for what to do here, which HTTP error.
                        when (isNothing l) (error "Header value larger than 4K bytes.")
                        bs <- Get.getByteString (fromJust l)
                        Get.skip 2 -- "\r\n"
                        getHeaders (f h bs)

-- Each header field consists of a name followed by a colon (":") and the 
-- field value. Field names are case-insensitive. The field value MAY be 
-- preceded by any amount of LWS, though a single SP is preferred. Header 
-- fields can be extended over multiple lines by preceding each extra line 
-- with at least one SP or HT. Applications ought to follow "common form", 
-- where one is known or indicated, when generating HTTP constructs, since 
-- there might exist some implementations that fail to accept anything.
-- I don't parse as case-insensitive, neither check for multiple white spaces.
getHeaderFromName :: BS8.ByteString -> Get.Get (Headers -> BS8.ByteString -> Headers)
getHeaderFromName peek =
        -- Ordered by use frequency (assumed by me).
        case peek of
                -- 4 options here.
                "Ac" -> do
                        Get.skip 6 -- "Accept"
                        peek' <- Get.lookAhead $ Get.getByteString 2
                        case peek' of
                                ": " -> do
                                        Get.skip 2 -- ": "
                                        return (\h bs -> h {accept = Just bs})
                                "-C" -> do
                                        Get.skip 10 -- "-Charset: "
                                        return (\h bs -> h {acceptCharset = Just bs})
                                "-E" -> do
                                        Get.skip 11 -- "-Encoding: "
                                        return (\h bs -> h {acceptEncoding = Just bs})
                                "-L" -> do
                                        Get.skip 11 -- "-Language: "
                                        return (\h bs -> h {acceptLanguage = Just bs})
                                _ -> return const
                -- 5 options here.
                "Co" -> do
                        Get.skip 2 -- "Co"/"Co"/"Co"
                        peek' <- Get.lookAhead $ Get.getByteString 2
                        case peek' of
                                "ok" -> do
                                        Get.skip 6 -- "okie: "
                                        return (\h bs -> h {cookie = Just bs})
                                "nn" -> do
                                        Get.skip 10 -- "nnection: "
                                        return (\h bs -> h {connection = Just bs})
                                "nt" -> do
                                        Get.skip 6 -- "ntent-"
                                        peek'' <- Get.lookAhead $ Get.getByteString 1
                                        case peek'' of
                                                "L" -> do
                                                        Get.skip 8-- "Length: "
                                                        return (\h bs -> h {contentLength = Just bs})
                                                "M" -> do
                                                        Get.skip 5 -- "MD5: "
                                                        return (\h bs -> h {contentMD5 = Just bs})
                                                "T" -> do
                                                        Get.skip 6 -- "Type: "
                                                        return (\h bs -> h {contentType = Just bs})
                                                _ -> return const
                                _ -> return const
                "Ho" -> do
                        Get.skip 6 -- "Host: "
                        return (\h bs -> h {host = Just bs})
                "Us" -> do
                        Get.skip 12 -- "User-Agent: "
                        return (\h bs -> h {userAgent = Just bs})
                -- 2 options here.
                "Re" -> do
                        Get.skip 9 -- "Referer: "
                        return (\h bs -> h {referer = Just bs})
                -- 2 options here.
                "Pr" -> do
                        Get.skip 2 -- "Pr"
                        peek' <- Get.lookAhead $ Get.getByteString 1
                        case peek' of
                                "a" -> do
                                        Get.skip 6 -- "agma: "
                                        return const -- TODO
                                "o" -> do
                                        Get.skip 19 -- "oxy-Authorization: "
                                        return const -- TODO
                                _ -> return const
                "Da" -> do
                        Get.skip 6 -- "Date: "
                        return const -- TODO
                "Ca" -> do
                        Get.skip 15 -- "Cache-Control: "                
                        return const -- TODO
                "Fr" -> do
                        Get.skip 6 -- "From: "
                        return const -- TODO
                "Ex" -> do
                        Get.skip 8 -- "Expect: "
                        return const -- TODO
                -- 5 options here.
                "If" -> do
                        Get.skip 3 -- "If-"
                        peek' <- Get.lookAhead $ Get.getByteString 2
                        case peek' of
                                "Ma" -> do
                                        Get.skip 7 -- "Match: "
                                        return const -- TODO
                                "Mo" -> do
                                        Get.skip 16 -- "Modified-Since: "
                                        return const -- TODO
                                "No" -> do
                                        Get.skip 12 -- "None-Match: "
                                        return const -- TODO
                                "Ra" -> do
                                        Get.skip 7 -- "Range: "
                                        return const -- TODO
                                "Un" -> do
                                        Get.skip 18 -- "Unmodified-Since: "
                                        return const -- TODO
                                _ -> return const
                "Au" -> do
                        Get.skip 15 -- "Authorization: "
                        return const -- TODO
                "Ma" -> do
                        Get.skip 14 -- "Max-Forwards: "
                        return const -- TODO
                "Wa" -> do
                        Get.skip 9 -- "Warning: "
                        return const -- TODO
                "Up" -> do
                        Get.skip 9 -- "Upgrade: "
                        return const -- TODO
                "TE" -> do
                        Get.skip 4 -- "TE: "
                        return const -- TODO
                "Ra" -> do
                        Get.skip 7 -- "Range: "
                        return const -- TODO
                "Vi" -> do
                        Get.skip 5 -- "Via: "
                        return const -- TODO
                _ -> return const

getBody :: Int -> Get.Get BS.ByteString
-- I don't care if the header says 0 or nothing but the body is not empty.
getBody contentLengthInt = do
        -- On getByteString if n <= 0 then the empty string is returned.
        bs <- Get.isolate contentLengthInt (Get.getByteString contentLengthInt)
        return bs
