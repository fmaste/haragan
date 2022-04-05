{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Response.Examples (
        google, googleResponse,
        amazon, amazonResponse,
        facebook, facebookResponse) where

--------------------------------------------------------------------------------

import qualified Data.Time as Time
import qualified Data.ByteString as BS
import qualified Network.HTTP.Response as R
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Version as Version

-- http://www.google.com.ar response received on Firefox (2013-08-13).
--------------------------------------------------------------------------------

google :: BS.ByteString
google = "HTTP/1.1 200 OK\r\nCache-Control: private, max-age=0\r\nContent-Encoding: gzip\r\nContent-Type: text/html; charset=UTF-8\r\nDate: Tue, 13 Aug 2013 16:31:44 GMT\r\nServer: gws\r\nTransfer-Encoding: chunked\r\nX-Frame-Options: SAMEORIGIN\r\n\r\n"

{-- 
The original was:
HTTP/1.1 200 OK
Date: Tue, 13 Aug 2013 16:31:44 GMT
Expires: -1
Cache-Control: private, max-age=0
Content-Type: text/html; charset=UTF-8
Content-Encoding: gzip
Server: gws
x-xss-protection: 1; mode=block
X-Frame-Options: SAMEORIGIN
Alternate-Protocol: 80:quic
Transfer-Encoding: chunked
--}


googleResponse :: R.Response
googleResponse = R.Response {
          R.version = Version.HTTP_1_1
        , R.status = Status.Status 200 "OK"
        , R.headers = googleHeaders
        , R.body = Nothing
}

googleHeaders :: R.Headers
googleHeaders = R.emptyHeaders {
          R.cacheControl = Just "private, max-age=0"
        , R.contentEncoding = Just "gzip"
        , R.contentType = Just "text/html; charset=UTF-8"
        , R.date = Just $ Time.buildTime Time.defaultTimeLocale [('Y',"2013"),('m',"08"),('d',"13"),('H',"16"),('M',"31"),('S',"44"),('Z',"GMT")]
        , R.server = Just "gws"
        , R.transferEncoding = Just "chunked"
        , R.xFrameOptions = Just "SAMEORIGIN"
}

-- http://www.amazon.com response received on Firefox (2013-08-15).
--------------------------------------------------------------------------------

amazon :: BS.ByteString
amazon = "HTTP/1.1 200 OK\r\nCache-Control: no-cache\r\nContent-Encoding: gzip\r\nContent-Type: text/html; charset=ISO-8859-1\r\nDate: Thu, 15 Aug 2013 16:46:37 GMT\r\nPragma: no-cache\r\nServer: Server\r\nTransfer-Encoding: chunked\r\n\r\n"

amazonResponse :: R.Response
amazonResponse = R.Response {
          R.version = Version.HTTP_1_1
        , R.status = Status.Status 200 "OK"
        , R.headers = amazonHeaders
        , R.body = Nothing
}

amazonHeaders :: R.Headers
amazonHeaders = R.emptyHeaders {
          R.cacheControl = Just "no-cache"
        , R.contentEncoding = Just "gzip"
        , R.contentType = Just "text/html; charset=ISO-8859-1"
        , R.date = Just $ Time.buildTime Time.defaultTimeLocale [('Y',"2013"),('m',"08"),('d',"15"),('H',"16"),('M',"46"),('S',"37"),('Z',"GMT")]
        , R.pragma = Just "no-cache"
        , R.server = Just "Server"
        , R.transferEncoding = Just "chunked"
}

{-- 
The original was:
HTTP/1.1 200 OK
Date: Thu, 15 Aug 2013 16:46:37 GMT
Server: Server
Set-Cookie: skin=noskin; path=/; domain=.amazon.com; expires=Thu, 15-Aug-2013 16:46:37 GMT
x-wl-uid=1GVT6MASDFSHI0ZgCXRHqgn/1zy+jo9vrXPqeCq/qruoVSYKNRt25IfNv2v1C/GmzokW2ugSXmbY=; path=/; domain=.amazon.com; expires=Tue, 01-Jan-2036 08:00:01 GMT
session-id-time=2082787201l; path=/; domain=.amazon.com; expires=Tue, 01-Jan-2036 08:00:01 GMT
session-id=181-2123617-0441242; path=/; domain=.amazon.com; expires=Tue, 01-Jan-2036 08:00:01 GMT
Pragma: no-cache
x-amz-id-1: 18F33CJ7RMDXPEHDZY93
p3p: policyref="http://www.amazon.com/w3c/p3p.xml",CP="CAO DSP LAW CUR ADM IVAo IVDo CONo OTPo OUR DELi PUBi OTRi BUS PHY ONL UNI PUR FIN COM NAV INT DEM CNT STA HEA PRE LOC GOV OTC "
Cache-Control: no-cache
Expires: -1
x-amz-id-2: TdQekHlTQI6avjURpGBVkUfVSv5cOTUq4/l+KL4waxLGHS2HqZcDsvQXwxlZ8YPz
Vary: Accept-Encoding,User-Agent
Content-Encoding: gzip
Content-Type: text/html; charset=ISO-8859-1
Transfer-Encoding: chunked
--}

-- http://www.facebook.com response received on Firefox (2013-08-15).
--------------------------------------------------------------------------------

facebook :: BS.ByteString
facebook = "HTTP/1.1 301 Moved Permanently\r\nCache-Control: private, no-cache, no-store, must-revalidate\r\nConnection: keep-alive\r\nContent-Length: 0\r\nContent-Type: text/html; charset=utf-8\r\nDate: Thu, 15 Aug 2013 17:47:03 GMT\r\nExpires: Sat, 01 Jan 2000 00:00:00 GMT\r\nLocation: https://www.facebook.com/\r\nPragma: no-cache\r\n\r\n"

facebookResponse :: R.Response
facebookResponse = R.Response {
          R.version = Version.HTTP_1_1
        , R.status = Status.Status 301 "Moved Permanently"
        , R.headers = facebookHeaders
        , R.body = Nothing
}

facebookHeaders :: R.Headers
facebookHeaders = R.emptyHeaders {
          R.cacheControl = Just "private, no-cache, no-store, must-revalidate"
        , R.connection = Just "keep-alive"
        , R.contentLength = Just "0"
        , R.contentType = Just "text/html; charset=utf-8"
        , R.date = Just $ Time.buildTime Time.defaultTimeLocale [('Y',"2013"),('m',"08"),('d',"15"),('H',"17"),('M',"47"),('S',"03"),('Z',"GMT")]
        , R.expires = Just $ Time.buildTime Time.defaultTimeLocale [('Y',"2000"),('m',"01"),('d',"01"),('H',"00"),('M',"00"),('S',"00"),('Z',"GMT")]
        , R.location = Just "https://www.facebook.com/"
        , R.pragma = Just "no-cache"
}

{-- 
The original was:
HTTP/1.1 301 Moved Permanently
Cache-Control: private, no-cache, no-store, must-revalidate
Expires: Sat, 01 Jan 2000 00:00:00 GMT
Location: https://www.facebook.com/
p3p: CP="Facebook does not have a P3P policy. Learn why here: http://fb.me/p3p"
Pragma: no-cache
x-content-type-options: nosniff
X-Frame-Options: DENY
Set-Cookie: datr=FxQNUvimajAr-vn0P-4Vlj_r; expires=Sat, 15-Aug-2015 17:47:03 GMT; path=/; domain=.facebook.com; httponly
wd=deleted; expires=Thu, 01-Jan-1970 00:00:01 GMT; path=/; domain=.facebook.com; httponly
Content-Type: text/html; charset=utf-8
X-FB-Debug: T1kBXffBuNGMM0E1+Uesfzi8WPSwHkrAYJFiXZPzJrY=
Date: Thu, 15 Aug 2013 17:47:03 GMT
Connection: keep-alive
Content-Length: 0
--}

