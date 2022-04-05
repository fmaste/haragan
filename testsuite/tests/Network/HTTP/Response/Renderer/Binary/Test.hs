{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Response.Renderer.Binary.Test (test) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Response as R
import qualified Network.HTTP.Response.Examples as E
import qualified Network.HTTP.Response.Renderer.Binary as B
import qualified Data.Binary.Put as Put
import qualified Test.HUnit as HU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

test :: Test
test = testGroup "Network.HTTP.Response.Renderer.Binary"
        [
                  testCase "Google response" googleTest
                , testCase "Amazon response" amazonTest
                , testCase "Facebook response" facebookTest
        ]

--------------------------------------------------------------------------------

runRenderer :: R.Response -> BS.ByteString
runRenderer res = BSL.toStrict $ Put.runPut (B.putResponse res)

--------------------------------------------------------------------------------

googleTest :: HU.Assertion
googleTest = HU.assertEqual "googleTest"
        E.google
        (runRenderer E.googleResponse) 

amazonTest :: HU.Assertion
amazonTest = HU.assertEqual "amazonTest"
        E.amazon
        (runRenderer E.amazonResponse) 

facebookTest :: HU.Assertion
facebookTest = HU.assertEqual "facebookTest"
        E.facebook
        (runRenderer E.facebookResponse) 

