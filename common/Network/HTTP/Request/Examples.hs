{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Request.Examples (
        chrome, chromeRequest,
        chromeFavicon, chromeFaviconRequest,
        firefox, firefoxRequest,
        firefoxFavicon, firefoxFaviconRequest,
        firefoxFaviconRetry, firefoxFaviconRetryRequest) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Network.HTTP.Request as R
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Version as Version

-- Chrome Version 28.0.1500.95 request.
--------------------------------------------------------------------------------

chrome :: BS.ByteString
chrome = "GET / HTTP/1.1\r\nHost: 127.0.0.1:12345\r\nConnection: keep-alive\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.95 Safari/537.36\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\n\r\n"

chromeRequest :: R.Request
chromeRequest = R.Request {
          R.method = Method.GET
        , R.uri = "/"
        , R.version = Version.HTTP_1_1
        , R.headers = chromeHeaders
        , R.body = Nothing
}

chromeHeaders :: R.Headers
chromeHeaders = R.Headers {
          R.host = Just "127.0.0.1:12345"
        , R.connection = Just "keep-alive"
        , R.contentLength = Nothing
        , R.contentType = Nothing
        , R.contentMD5 = Nothing
        , R.accept = Just "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        , R.acceptCharset = Nothing
        , R.acceptEncoding = Just "gzip,deflate,sdch"
        , R.acceptLanguage = Just "en-US,en;q=0.8"
        , R.userAgent = Just "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.95 Safari/537.36"
        , R.referer = Nothing
        , R.cookie = Nothing
}

-- Chrome Version 28.0.1500.95 favicon request.
--------------------------------------------------------------------------------

chromeFavicon :: BS.ByteString
chromeFavicon = "GET /favicon.ico HTTP/1.1\r\nHost: 127.0.0.1:12345\r\nConnection: keep-alive\r\nAccept: */*\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.95 Safari/537.36\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\n\r\n"

chromeFaviconRequest :: R.Request
chromeFaviconRequest = R.Request {
          R.method = Method.GET
        , R.uri = "/favicon.ico"
        , R.version = Version.HTTP_1_1
        , R.headers = chromeFaviconHeaders
        , R.body = Nothing
}

chromeFaviconHeaders :: R.Headers
chromeFaviconHeaders = R.Headers {
          R.host = Just "127.0.0.1:12345"
        , R.connection = Just "keep-alive"
        , R.contentLength = Nothing
        , R.contentType = Nothing
        , R.contentMD5 = Nothing
        , R.accept = Just "*/*"
        , R.acceptCharset = Nothing
        , R.acceptEncoding = Just "gzip,deflate,sdch"
        , R.acceptLanguage = Just "en-US,en;q=0.8"
        , R.userAgent = Just "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.95 Safari/537.36"
        , R.referer = Nothing
        , R.cookie = Nothing
}

-- Firefox Version 23.0 request.
--------------------------------------------------------------------------------

firefox :: BS.ByteString
firefox = "GET / HTTP/1.1\r\nHost: 127.0.0.1:12345\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nConnection: keep-alive\r\n\r\n"

firefoxRequest :: R.Request
firefoxRequest = R.Request {
          R.method = Method.GET
        , R.uri = "/"
        , R.version = Version.HTTP_1_1
        , R.headers = firefoxHeaders
        , R.body = Nothing
}

firefoxHeaders :: R.Headers
firefoxHeaders = R.Headers {
          R.host = Just "127.0.0.1:12345"
        , R.connection = Just "keep-alive"
        , R.contentLength = Nothing
        , R.contentType = Nothing
        , R.contentMD5 = Nothing
        , R.accept = Just "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        , R.acceptCharset = Nothing
        , R.acceptEncoding = Just "gzip, deflate"
        , R.acceptLanguage = Just "en-US,en;q=0.5"
        , R.userAgent = Just "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0"
        , R.referer = Nothing
        , R.cookie = Nothing
}

-- Firefox Version 23.0 favicon request.
--------------------------------------------------------------------------------

firefoxFavicon :: BS.ByteString
firefoxFavicon = "GET /favicon.ico HTTP/1.1\r\nHost: 127.0.0.1:12345\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0\r\nAccept: image/png,image/*;q=0.8,*/*;q=0.5\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nConnection: keep-alive\r\n\r\n"

firefoxFaviconRequest :: R.Request
firefoxFaviconRequest = R.Request {
          R.method = Method.GET
        , R.uri = "/favicon.ico"
        , R.version = Version.HTTP_1_1
        , R.headers = firefoxFaviconHeaders
        , R.body = Nothing
}

firefoxFaviconHeaders :: R.Headers
firefoxFaviconHeaders = R.Headers {
          R.host = Just "127.0.0.1:12345"
        , R.connection = Just "keep-alive"
        , R.contentLength = Nothing
        , R.contentType = Nothing
        , R.contentMD5 = Nothing
        , R.accept = Just "image/png,image/*;q=0.8,*/*;q=0.5"
        , R.acceptCharset = Nothing
        , R.acceptEncoding = Just "gzip, deflate"
        , R.acceptLanguage = Just "en-US,en;q=0.5"
        , R.userAgent = Just "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0"
        , R.referer = Nothing
        , R.cookie = Nothing
}

-- Firefox Version 23.0 favicon request retry.
--------------------------------------------------------------------------------

firefoxFaviconRetry :: BS.ByteString
firefoxFaviconRetry = "GET /favicon.ico HTTP/1.1\r\nHost: 127.0.0.1:12345\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nConnection: keep-alive\r\n\r\n"

firefoxFaviconRetryRequest :: R.Request
firefoxFaviconRetryRequest = R.Request {
          R.method = Method.GET
        , R.uri = "/favicon.ico"
        , R.version = Version.HTTP_1_1
        , R.headers = firefoxFaviconRetryHeaders
        , R.body = Nothing
}

firefoxFaviconRetryHeaders :: R.Headers
firefoxFaviconRetryHeaders = R.Headers {
          R.host = Just "127.0.0.1:12345"
        , R.connection = Just "keep-alive"
        , R.contentLength = Nothing
        , R.contentType = Nothing
        , R.contentMD5 = Nothing
        , R.accept = Just "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        , R.acceptCharset = Nothing
        , R.acceptEncoding = Just "gzip, deflate"
        , R.acceptLanguage = Just "en-US,en;q=0.5"
        , R.userAgent = Just "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:23.0) Gecko/20100101 Firefox/23.0"
        , R.referer = Nothing
        , R.cookie = Nothing
}
