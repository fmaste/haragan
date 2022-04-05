{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Session.Test (test) where

--------------------------------------------------------------------------------

import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Data.List (sort)
import qualified Data.ByteString.Char8 as BSS
import qualified Network.HTTP.Cookie as C
import qualified Network.HTTP.Session as S
import qualified Test.HUnit as HU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

type Session = Int

type Storage = S.Storage Session

name :: BSS.ByteString
name = "ssid"

cookie :: C.Name -> C.Value -> BSS.ByteString
cookie n v = n <> "=" <> v

--------------------------------------------------------------------------------

test :: Test
test = testGroup "Network.HTTP.Session" 
        [
                  testCase "Create and get a session" createAndGet
                , testCase "Create, set and get a session" createSetAndGet
                , testCase "Create, delete and get a session" createDeleteAndGet
                , testCase "Create, set, delete and get a multiple sessions" concurrentSession
        ]

createAndGet :: HU.Assertion
createAndGet = do
        s <- S.newStorage name
        header <- S.newSession s 0 C.emptyParams
        mi <- S.getSession s $ cookie name (fromJust $ C.getValue name header)
        HU.assertEqual "createAndGet"
                (Just (0 :: Session))
                mi

createSetAndGet :: HU.Assertion
createSetAndGet = do
        s <- S.newStorage name
        header <- S.newSession s 0 C.emptyParams
        S.setSession s 1 $ cookie name (fromJust $ C.getValue name header)
        mi <- S.getSession s $ cookie name (fromJust $ C.getValue name header)
        HU.assertEqual "createSetAndGet"
                (Just (1 :: Session))
                mi

createDeleteAndGet :: HU.Assertion
createDeleteAndGet = do
        s <- S.newStorage name
        header <- S.newSession s 0 C.emptyParams
        S.deleteSession s $ cookie name (fromJust $ C.getValue name header)
        mi <- S.getSession s $ cookie name (fromJust $ C.getValue name header)
        HU.assertEqual "createDeleteAndGet"
                (Nothing :: Maybe Session)
                mi

concurrentSession :: HU.Assertion
concurrentSession = do
        s <- S.newStorage name
        c <- newChan
        _ <- mapM (\i -> forkIO $ threadDo i s c) [0..99]
        is <- replicateM 100 (readChan c)
        HU.assertEqual "concurrentSession"
                [1..100]
                (sort is)

threadDo :: Session -> Storage -> Chan Session -> IO ()
threadDo i s c = do
        header <- S.newSession s i C.emptyParams
        S.setSession s (i+1) $ cookie name (fromJust $ C.getValue name header)
        mi <- S.getSession s $ cookie name (fromJust $ C.getValue name header)
        S.deleteSession s $ cookie name (fromJust $ C.getValue name header)
        mi' <- S.getSession s $ cookie name (fromJust $ C.getValue name header)
        when (isNothing mi || isJust mi') (error "Thread error.")
        writeChan c (fromJust mi)
