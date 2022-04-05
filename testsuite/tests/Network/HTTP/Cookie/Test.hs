{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Cookie.Test (test) where

--------------------------------------------------------------------------------

import qualified Data.Time as Time
import qualified Network.HTTP.Cookie as C
import qualified Test.HUnit as HU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

test :: Test
test = testGroup "Network.HTTP.Cookie" 
        [
                  testCase "Get value on no cookies header" noCookiesHeader
                , testCase "Get value on empty cookies header" emptyCookiesHeader
                , testCase "Get value from a single cookie" valueOne
                , testCase "Get first value from two cookies" valueTwoOne
                , testCase "Get second value from two cookies" valueTwoTwo
                , testCase "Get first value from three cookies" valueThreeOne
                , testCase "Get second value from three cookies" valueThreeTwo
                , testCase "Get last value from three cookies" valueThreeThree
                , testCase "Get value from a repeated cookie" valueRepeat
                , testCase "Get value from a complicated value" valueComplicated
                , testCase "Set cookie with no params" setNone
                , testCase "Set cookie with domain param" setDomain
                , testCase "Set cookie with path param" setPath
                , testCase "Set cookie with expires param 1" setExpires1
                , testCase "Set cookie with expires param 2" setExpires2
                , testCase "Set cookie with expires param 3" setExpires3
                , testCase "Set cookie with max-age param" setMaxAge
                , testCase "Set cookie with secure param" setSecure
                , testCase "Set cookie with http-only param" setHttpOnly
                , testCase "Set cookie with all param together" setAll
        ]

noCookiesHeader :: HU.Assertion
noCookiesHeader = HU.assertEqual "noCookiesHeader"
        Nothing
        (C.getValue "c" "")

emptyCookiesHeader :: HU.Assertion
emptyCookiesHeader = HU.assertEqual "emptyCookiesHeader"
        Nothing
        (C.getValue "c" "")

valueOne :: HU.Assertion
valueOne = HU.assertEqual "valueOne"
        (Just "31d4d96e407aad42")
        (C.getValue "c" "c=31d4d96e407aad42")

valueTwoOne :: HU.Assertion
valueTwoOne = HU.assertEqual "valueTwoOne"
        (Just "31d4d96e407aad42")
        (C.getValue "c" "c=31d4d96e407aad42; lang=en_US")

valueTwoTwo :: HU.Assertion
valueTwoTwo = HU.assertEqual "valueTwoTwo"
        (Just "en_US")
        (C.getValue "lang" "c=31d4d96e407aad42; lang=en_US")

valueThreeOne :: HU.Assertion
valueThreeOne = HU.assertEqual "valueThreeOne"
        (Just "31d4d96e407aad42")
        (C.getValue "c" "c=31d4d96e407aad42; lang=en_US; d=hello")

valueThreeTwo :: HU.Assertion
valueThreeTwo = HU.assertEqual "valueTwoTwo"
        (Just "en_US")
        (C.getValue "lang" "c=31d4d96e407aad42; lang=en_US; d=hello")

valueThreeThree :: HU.Assertion
valueThreeThree = HU.assertEqual "valueThreeThree"
        (Just "hello")
        (C.getValue "d" "c=31d4d96e407aad42; lang=en_US; d=hello")

valueRepeat :: HU.Assertion
valueRepeat = HU.assertEqual "valueRepat"
        (Just "31d4d96e407aad42")
        (C.getValue "c" "c=31d4d96e407aad42; c=en_US")

valueComplicated :: HU.Assertion
valueComplicated = HU.assertEqual "valueComplicated"
        (Just "HNaqS}}RKDgdJg\\oFjun@AHcklGWLtgs]b?H~xnALwQK\\UeU~i?Q\\CMu|Oq]BfvyrB^^_jNjxRARLuh_^IFRNZdDFZHcQYBjq{gYyojw~bSV|[`fZjprzbZa}l^XchYt")
        (C.getValue "c" "c=HNaqS}}RKDgdJg\\oFjun@AHcklGWLtgs]b?H~xnALwQK\\UeU~i?Q\\CMu|Oq]BfvyrB^^_jNjxRARLuh_^IFRNZdDFZHcQYBjq{gYyojw~bSV|[`fZjprzbZa}l^XchYt; c=en_US")

setNone :: HU.Assertion
setNone = HU.assertEqual "setNone"
        "name=value"
        (C.setCookieValue "name" "value" C.emptyParams)

setDomain :: HU.Assertion
setDomain = HU.assertEqual "setDomain"
        "name=value; Domain=www.example.com"
        (C.setCookieValue "name" "value" (C.emptyParams {C.domain = Just "www.example.com"}))

setPath :: HU.Assertion
setPath = HU.assertEqual "setPath"
        "name=value; Path=/"
        (C.setCookieValue "name" "value" (C.emptyParams {C.path = Just "/"}))

setExpires1 :: HU.Assertion
setExpires1 = HU.assertEqual "setExpires2"
        "name=value; Expires=Thu, 01 Jan 1970 00:00:01 GMT"
        (C.setCookieValue "name" "value" (C.emptyParams {C.expires = Just expires1}))

expires1 :: Time.UTCTime
expires1 = Time.buildTime Time.defaultTimeLocale 
        [('Y',"1970"), ('m',"01"), ('d',"01"), ('H',"00"), ('M',"00"), ('S',"01")]

setExpires2 :: HU.Assertion
setExpires2 = HU.assertEqual "setExpires1"
        "name=value; Expires=Tue, 22 Apr 1986 12:59:59 GMT"
        (C.setCookieValue "name" "value" (C.emptyParams {C.expires = Just expires2}))

expires2 :: Time.UTCTime
expires2 = Time.buildTime Time.defaultTimeLocale 
        [('Y',"1986"), ('m',"04"), ('d',"22"), ('H',"12"), ('M',"59"), ('S',"59")]

setExpires3 :: HU.Assertion
setExpires3 = HU.assertEqual "setExpires1"
        "name=value; Expires=Tue, 15 Jan 2013 21:47:38 GMT"
        (C.setCookieValue "name" "value" (C.emptyParams {C.expires = Just expires3}))

expires3 :: Time.UTCTime
expires3 = Time.buildTime Time.defaultTimeLocale 
        [('Y',"2013"), ('m',"01"), ('d',"15"), ('H',"21"), ('M',"47"), ('S',"38")]

setMaxAge :: HU.Assertion
setMaxAge = HU.assertEqual "setMaxAge"
        "name=value; Max-Age=10"
        (C.setCookieValue "name" "value" (C.emptyParams {C.maxAge = Just 10}))

setSecure :: HU.Assertion
setSecure = HU.assertEqual "setSecure"
        "name=value; Secure"
        (C.setCookieValue "name" "value" (C.emptyParams {C.secure = True}))

setHttpOnly :: HU.Assertion
setHttpOnly = HU.assertEqual "setHttpOnly"
        "name=value; HttpOnly"
        (C.setCookieValue "name" "value" (C.emptyParams {C.httpOnly = True}))

setAll :: HU.Assertion
setAll = HU.assertEqual "setAll"
        "name=value; Domain=example.com; Path=/images; Expires=Wed, 13 Feb 2013 15:10:38 GMT; Max-Age=99; Secure; HttpOnly"
        (C.setCookieValue "name" "value"
                        (C.Params
                                {
                                          C.domain = Just "example.com"
                                        , C.path = Just "/images"
                                        , C.expires = Just expiresAll
                                        , C.maxAge = Just 99
                                        , C.secure = True
                                        , C.httpOnly = True
                                }
                        )
        )

expiresAll :: Time.UTCTime
expiresAll = Time.buildTime Time.defaultTimeLocale
        [('Y',"2013"), ('m',"02"), ('d',"13"), ('H',"15"), ('M',"10"), ('S',"38")]
