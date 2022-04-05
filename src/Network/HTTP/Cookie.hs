{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Cookie (
        Name,
        Value,
        Domain,
        Path,
        ------------------------
        getValue,
        ------------------------
        Params (Params),
        domain, path, expires, maxAge, secure, httpOnly,
        emptyParams,
        ------------------------
        setCookieValue) where

--------------------------------------------------------------------------------

import Data.String
import Data.Time
import qualified Data.ByteString.Char8 as BS8

--------------------------------------------------------------------------------

-- RFC 6265
-- The value of a cookie may consist of any printable ascii character 
-- ("!" to "~", unicode \u0021 to \u007E) excluding "," and ";" and whitespace.
-- The name of the cookie also excludes "=", name and value delimiter.  
-- The cookie standard RFC2965 is more limiting but not implemented by browsers.

type Name = BS8.ByteString

type Value = BS8.ByteString

type Domain = BS8.ByteString

type Path = BS8.ByteString

--------------------------------------------------------------------------------

{--
-- TODO: Trim??????

import Data.Char

trim :: BS8.ByteString -> BS8.ByteString
trim bs = 
        BS8.takeWhile (not . isSpace) $ 
                BS8.dropWhile isSpace bs
--}

--------------------------------------------------------------------------------

getValue :: Name -> BS8.ByteString -> Maybe Value
getValue name bs = if BS8.null bs
        then Nothing
        else lookup name $ splitCookies bs

splitCookies :: BS8.ByteString -> [(Name, Value)]
splitCookies bs = splitCookies' $ map (BS8.span (/= '=')) $ BS8.split ';' bs

-- Examples:
-- "c=31d4d96e407aad42" -> [("c","=31d4d96e407aad42")]
-- "c=31d4d96e407aad42; lang=en-US" -> [("c","=31d4d96e407aad42"),(" lang","=en-US")]
splitCookies' :: [(Name, Value)] -> [(Name, Value)]
splitCookies' [] = []
splitCookies' ((n,v):[]) = [(n, BS8.drop 1 v)]
splitCookies' ((n,v):xs) = (n, BS8.drop 1 v) : [(BS8.drop 1 n', BS8.drop 1 v') | (n',v') <- xs]

--------------------------------------------------------------------------------

data Params = Params {
        domain :: Maybe Domain,
        path :: Maybe Path,
        expires :: Maybe UTCTime,
        maxAge :: Maybe Int,
        secure :: Bool,
        httpOnly :: Bool
}

emptyParams :: Params
emptyParams = Params {
        domain = Nothing,
        path = Nothing,
        expires = Nothing,
        maxAge = Nothing,
        secure = False,
        httpOnly = False
}

--------------------------------------------------------------------------------

setCookieValue :: Name -> Value -> Params -> BS8.ByteString
setCookieValue name value cp = name <> "=" <> value <> params cp

params :: Params -> BS8.ByteString
params cp =
        paramsGeneric (domain cp) "Domain" id <>
        paramsGeneric (path cp) "Path" id <>
        paramsGeneric (expires cp) "Expires"
                -- Time locale: RFC 1123 (an update to RFC 822).
                -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html
                (\t -> BS8.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %T GMT" t) <>
        paramsGeneric (maxAge cp) "Max-Age" (fromString . show) <>
        (if secure cp then "; Secure" else "") <>
        (if httpOnly cp then "; HttpOnly" else "")

paramsGeneric :: Maybe a -> BS8.ByteString -> (a -> BS8.ByteString) -> BS8.ByteString
paramsGeneric  Nothing _ _ = mempty
paramsGeneric  (Just x) name f = "; " <> name <> "=" <> f x
