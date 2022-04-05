{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Server (start) where

--------------------------------------------------------------------------------

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Network.HTTP.Request.Parser.Binary as RequestParser
import qualified Network.HTTP.Request as Request
import qualified Network.HTTP.Response.Renderer.Binary as ResponseRenderer
import qualified Network.HTTP.Response as Response
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Version as Version
import qualified Network.TCP.Server as TCPServer

--------------------------------------------------------------------------------

start :: Int -> (Request.Request -> IO Response.Response) -> IO ()
start port userAction = do
        eitherServer <- TCPServer.newServer port
        case eitherServer of
                Left err -> error (show err)
                Right server -> do
                        _ <- forkIO (TCPServer.accepterProducer server)
                        TCPServer.accepterConsumer server (action userAction)

action :: (Request.Request -> IO Response.Response) -> TCPServer.Client -> IO ()
action userAction client = do
        eitherRequest <- parseRequest (Get.runGetIncremental RequestParser.getRequest) client
        response <- case eitherRequest of
                Left errBs -> return $
                        Response.Response {
                                Response.version = Version.HTTP_1_1,
                                Response.status = Status.Status 400 "Bad Request",
                                Response.headers = Response.emptyHeaders,
                                Response.body = Just errBs
                        }
                Right request -> do
                        catch 
                                (userAction request)
                                (\e -> return $ 
                                        Response.Response {
                                                Response.version = Version.HTTP_1_1,
                                                Response.status = Status.Status 500 "Internal Server Error",
                                                Response.headers = Response.emptyHeaders,
                                                Response.body = Just (BS8.pack $ show (e :: SomeException))
                                        }
                                )
        TCPServer.send client (BSL.toStrict $ Put.runPut $ ResponseRenderer.putResponse response)
        -- Always close the connection when finished.
        TCPServer.close client
        return ()

parseRequest :: Get.Decoder Request.Request -> TCPServer.Client -> IO (Either BS.ByteString Request.Request)
parseRequest decoder client = do
        case decoder of
                -- And error occured, a malformed request was found.
                Get.Fail _ _ errStr -> return (Left $ BS8.pack errStr)
                -- More input needed.
                Get.Partial continue -> do
                        -- TODO: Add a timeout here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        bs <- TCPServer.recv client 4096
                        if BS.null bs
                                then case (Get.pushEndOfInput decoder) of
                                        -- Client closed its half side of the connection.
                                        Get.Partial _ -> error "Client connection closed with a partial request."
                                        decoder' -> parseRequest decoder' client
                                else parseRequest (continue (Just bs)) client
                Get.Done _ _ ans -> return (Right ans)
