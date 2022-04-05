{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.TCP.Server (
          Server
        , newServer
        , Internal.Error (..)
        , Client
        , accepterProducer, accepterConsumer
        , send, recv
        , close
) where

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import qualified Network.TCP.Server.Internal as Internal

-- The server abstraction.
--------------------------------------------------------------------------------

data Server = Server {
          serverSocket :: (MVar Internal.SocketServer)
        , portNumber :: Internal.PortNumber
        , clientsChan :: Chan Internal.SocketClient
        , consumerThreadId :: Maybe ThreadId
}

{-|
        Creates an already bounded and listening server.
-}
newServer :: Int -> IO (Either Internal.Error Server)
newServer portNum = do
        accepterSocketAns <- Internal.createServer (fromIntegral portNum)
        case accepterSocketAns of
                Left err -> return $ Left err
                Right socket -> do
                        server <- newServer' socket
                        return $ Right server

newServer' :: Internal.SocketServer -> IO Server
newServer' socket = do
        mVar <- newMVar socket
        port <- Internal.serverPort socket
        chan <- newChan
        return $ Server mVar port chan Nothing

-- TODO Server functions
--------------------------------------------------------------------------------

{--

serverPort :: Server -> Internal.PortNumber
serverPort = portNumber

startServer :: Server -> (S.Socket -> S.SockAddr -> IO ()) -> IO ()
startServer server f = do

{-|
        A server can be paused and resumed.
        When paused the server does not accept new connections, are kept on hold.
        The ones that are being processed are a mistery to this function.
-}
pauseServer :: Server -> IO ()

{-|
        Continue consuming newly created connections and the ones waiting
-}
resumeServer :: Server -> IO ()

{-|
-}
serverPaused :: Server -> IO Bool

{-|
        A server can be paused and resumed.
        When paused the server does not accept new connections, are kept on hold.
        The ones that are being processed are a mistery to this function.
-}
endServer :: Server -> IO ()
endServer server = do

--}

-- The clients abstraction.
--------------------------------------------------------------------------------

data Client = Client {
        clientSocket :: Internal.SocketClient
}

{-|
        Accept new connections.
        The listening socket is read from an MVar.
        Every new accepted connection is added untouched to a FIFO chan.
-}
accepterProducer ::  Server -> IO ()
accepterProducer (Server acceptSem _ acceptQueue _) =
        forever $ do
                -- withMVar is only atomic if there are no other producers for this MVar.
                withMVar acceptSem $ \srvrSocket -> do
                        tmp <- Internal.acceptClient srvrSocket
                        writeChan acceptQueue tmp

--------------------------------------------------------------------------------

{-|
        Reads new accepted connection from the Chan.
-}
accepterConsumer :: Server -> (Client -> IO ()) -> IO ()
accepterConsumer (Server _ _ acceptQueue _) handler = do
        forever $ do
                cliSocket <- readChan acceptQueue
                threadId <- forkIO $ handler (Client cliSocket)
                return ()

--------------------------------------------------------------------------------

send :: Client -> BS.ByteString -> IO ()
send client@(Client socket) bs = do
        sentBytes <- Internal.send socket bs
        when (sentBytes < BS.length bs) (send client (BS.drop sentBytes bs))

recv :: Client -> Int -> IO BS.ByteString
recv (Client socket) = Internal.recv socket

close :: Client -> IO ()
close (Client socket) = Internal.destroyClient socket
