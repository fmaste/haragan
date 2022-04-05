{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Test.Framework (Test, defaultMain)
import Network.HTTP.Cookie.Test as Cookie
import Network.HTTP.Request.Parser.Binary.Test as RequestParserBinary 
import Network.HTTP.Response.Renderer.Binary.Test as ResponseRendererBinary 
import Network.TCP.Server.Internal.Test as TCPServer
import Network.HTTP.Session.Test as Session

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------

tests :: [Test]
tests =
        [
                  Cookie.test
                , RequestParserBinary.test
                , ResponseRendererBinary.test
                , TCPServer.test
                , Session.test
        ]
