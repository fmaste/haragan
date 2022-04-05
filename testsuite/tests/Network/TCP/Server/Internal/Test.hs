{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.TCP.Server.Internal.Test (test) where

--------------------------------------------------------------------------------

import qualified Network.TCP.Server.Internal as Internal
import qualified Test.HUnit as HU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

test :: Test
test = testGroup "Network.HTTP.Server.Internal"
        [
                  testCase "Create a socket on a random port" createServer
                , testCase "Creating a socket on a port lower than 1024 must fail" permissionDenied
                , testCase "Creating a socket on the same port must fail" addressAlreadyInUse
        ]

createServer :: HU.Assertion
createServer = do
        eitherSocket <- Internal.createServer 0
        case eitherSocket of
                Left err -> HU.assertFailure (show err)
                Right serverSocket -> do
                        Internal.destroyServer serverSocket
                        return ()

permissionDenied :: HU.Assertion
permissionDenied = do
        eitherSocket <- Internal.createServer 1
        case eitherSocket of
                Left Internal.SocketPermissionDenied -> return ()
                Left err -> HU.assertFailure (show err)
                Right serverSocket -> do
                        Internal.destroyServer serverSocket
                        HU.assertFailure "Creating a socket on a port lower than 1024 succeded."

addressAlreadyInUse :: HU.Assertion
addressAlreadyInUse = do
        eitherSocket <- Internal.createServer 0
        case eitherSocket of
                Left err -> HU.assertFailure (show err)
                Right serverSocket -> do
                        port <- Internal.serverPort serverSocket
                        eitherSocket' <- Internal.createServer port
                        Internal.destroyServer serverSocket
                        case eitherSocket' of
                                Left Internal.SocketResourceBusy -> return ()
                                Left err -> HU.assertFailure (show err)
                                Right serverSocket' -> do
                                        Internal.destroyServer serverSocket'
                                        HU.assertFailure "Rebind did not fail."

