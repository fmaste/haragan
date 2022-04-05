{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Server (start)
import qualified Network.HTTP.Request as Request
import qualified Network.HTTP.Response as Response
import qualified Network.HTTP.Types.Status as Status

main :: IO ()
main = start 8080 action

action :: Request.Request -> IO Response.Response
action req = do
        let res = Response.Response {
                  Response.version = Request.version req
                , Response.status = Status.Status 200 "OK"
                , Response.headers = Response.emptyHeaders
                , Response.body = Request.body req
        }
        return res

