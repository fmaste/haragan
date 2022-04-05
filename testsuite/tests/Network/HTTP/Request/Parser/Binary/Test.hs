{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Request.Parser.Binary.Test (test) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Request as R
import qualified Network.HTTP.Request.Examples as E
import qualified Network.HTTP.Request.Parser.Binary as B
import qualified Data.Binary.Get as Get
import qualified Test.HUnit as HU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

test :: Test
test = testGroup "Network.HTTP.Request.Parser.Binary" 
        [
                  testCase "Chrome Version 28.0.1500.95 request" chromeTest
                , testCase "Chrome Version 28.0.1500.95 favicon request" chromeFaviconTest
                , testCase "Firefox Version 23.0 request" firefoxTest
                , testCase "Firefox Version 23.0 favicon request" firefoxFaviconTest
                , testCase "Firefox Version 23.0 favicon request retry" firefoxFaviconRetryTest
        ]

--------------------------------------------------------------------------------

runParser :: BS.ByteString -> Either String R.Request
runParser str =
        case Get.runGetOrFail B.getRequest (BSL.fromStrict str) of
                Left (_,_,err) -> Left err
                Right (_,_,ans) -> Right ans

--------------------------------------------------------------------------------

chromeTest :: HU.Assertion
chromeTest = HU.assertEqual "chromeTest"
        (Right E.chromeRequest)
        (runParser E.chrome)

--------------------------------------------------------------------------------

chromeFaviconTest :: HU.Assertion
chromeFaviconTest = HU.assertEqual "chromeFaviconTest"
        (Right E.chromeFaviconRequest)
        (runParser E.chromeFavicon)

--------------------------------------------------------------------------------

firefoxTest :: HU.Assertion
firefoxTest = HU.assertEqual "firefoxTest"
        (Right E.firefoxRequest)
        (runParser E.firefox)

--------------------------------------------------------------------------------

firefoxFaviconTest :: HU.Assertion
firefoxFaviconTest = HU.assertEqual "firefoxFaviconTest"
        (Right E.firefoxFaviconRequest)
        (runParser E.firefoxFavicon)

--------------------------------------------------------------------------------

firefoxFaviconRetryTest :: HU.Assertion
firefoxFaviconRetryTest = HU.assertEqual "firefoxFavicon2Test"
        (Right E.firefoxFaviconRetryRequest)
        (runParser E.firefoxFaviconRetry)
