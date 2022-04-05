{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.TCP.Server.Internal (
          SocketServer
        , SocketClient
        , PortNumber
        , Error (..)
        , createServer
        , destroyServer
        , serverPort
        , acceptClient, destroyClient
        , send, recv
) where

--------------------------------------------------------------------------------

import Data.Word (Word16)
import Control.Exception
import Control.Monad
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

--------------------------------------------------------------------------------

data SocketServer = SocketServer {
          serverSocket :: S.Socket
        , socketAddrInfo :: S.AddrInfo
}

data SocketClient = SocketClient {
          clientSocket :: S.Socket
        , socketAddr :: S.SockAddr
}

type PortNumber = Word16

data Error =
        FamilyIPv6NotSupported |
        TypeStreamNotSupported |
        FlagAiPassiveNotImplemented |
        FlagAiNumericServNotImplemented |
        AddrInfoNotFound |
        AddrInfoAmbiguous [S.AddrInfo] |
        SocketPermissionDenied |
        SocketResourceBusy |
        Unknown IOException
        deriving (Show)

--------------------------------------------------------------------------------

createServer :: 
        PortNumber
        -- ^ Port 0 means use the next available port.
        -> IO (Either Error SocketServer) 
        -- ^ Returns either the error or a socket prepared to accept clients.
{-|
        Create an IPv6 socket (v4 compatible) and start to listen for new connections.
        Only known IOExceptions are catched, otherwise cleanup and rethrow is done.
-}
createServer portNumber
        -- First check all the static things I can.
        | (not $ S.isSupportedFamily S.AF_INET6) = 
                return $ Left FamilyIPv6NotSupported
        | (not $ S.isSupportedSocketType S.Stream) = 
                return $ Left TypeStreamNotSupported
        | (not $ S.addrInfoFlagImplemented S.AI_PASSIVE) = 
                return $ Left FlagAiPassiveNotImplemented
        | (not $ S.addrInfoFlagImplemented S.AI_NUMERICSERV) = 
                return $ Left FlagAiNumericServNotImplemented
        | otherwise = do
                -- The withSocketsDo function is only necessary for Windows.
                -- Initializes Windows winsock stuff and does nothing on other systems.
                ans <- S.withSocketsDo (createSocket (toEnum $ fromEnum portNumber))
                case ans of
                        Right socket -> return $ Right socket
                        Left err -> return $ Left err

createSocket :: S.PortNumber -> IO (Either Error SocketServer) 
createSocket portNumber = do
        -- Find a suitable unique addrinfo structure.
        addrInfos <- getAddrInfos portNumber
        case addrInfos of
                -- No addinfo.
                ([]) -> return $ Left AddrInfoNotFound
                -- Multiple addrinfo structures.
                t@(_:_:_) -> return $ Left $ AddrInfoAmbiguous t
                -- Only one!!
                (addrInfo:[]) -> do
                        -- Create the socket.
                        -- TODO: Assuming parameters retrieved from addinfo are error free.
                        -- TODO: Other errors (http://man7.org/linux/man-pages/man2/socket.2.html)
                        socket <- S.socket 
                                (S.addrFamily addrInfo) 
                                (S.addrSocketType addrInfo) 
                                (S.addrProtocol addrInfo)       
                        catch 
                                (do
                                        -- TODO: Use setSocketOption to customize the socket.
                                        -- Allow reusing the socket address, so when the socket is 
                                        -- not closed correctly avoid the SocketResourceBusy error.
                                        when 
                                                (S.isSupportedSocketOption S.ReuseAddr) 
                                                (S.setSocketOption socket S.ReuseAddr 1)
                                        -- TODO: Before check the options using isSupportedSocketOption.
                                        -- bind() assigns the address specified by addr to the socket.
                                        S.bind socket (S.addrAddress addrInfo)
                                        -- listen() marks the socket referred to by sockfd as a passive socket, 
                                        -- that is, as a socket that will be used to accept incoming connection 
                                        -- requests using accept(2).
                                        -- The second parameter is the listen backlog: it specifies the queue 
                                        -- length for completely established sockets waiting to be accepted.
                                        -- http://veithen.blogspot.com.ar/2014/01/how-tcp-backlog-works-in-linux.html
                                        -- If the backlog is greater than the /proc/sys/net/core/somaxconn
                                        -- then it is silently  truncated  to  that  value.
                                        S.listen socket maxBound
                                        -- The AddrInfo is not returned, I found it useless.
                                        -- For example if you create a socket with port 0 using an addr with port 0
                                        -- the socket is assigned a random free port that can only be obtained by
                                        -- querying the socket.
                                        return $ Right (SocketServer socket addrInfo)
                                )
                                (\e -> do
                                        S.close socket
                                        if isPermissionError e 
                                                then return $ Left SocketPermissionDenied
                                                else if isAlreadyInUseError e
                                                        then return $ Left SocketResourceBusy
                                                        else throwIO e
                                )

-- Given node and service, which identify an Internet host and a service, 
-- getaddrinfo() returns one or more addrinfo structures, each of which 
-- contains an Internet address that can be specified in a call to bind(2) 
-- or connect(2).
-- http://linux.die.net/man/3/getaddrinfo
getAddrInfos :: S.PortNumber -> IO [S.AddrInfo]
getAddrInfos portNumber = S.getAddrInfo
        -- The hints argument points to an addrinfo structure that specifies 
        -- criteria for selecting the socket address structures returned.
        (Just $ S.defaultHints 
                {
                        -- This field specifies additional options.
                        S.addrFlags =
                                [
                                        -- If the AI_PASSIVE flag is specified, and node is NULL, 
                                        -- then the returned socket addresses will be suitable 
                                        -- for binding a socket that will accept connections. 
                                        -- The returned socket address will contain the "wildcard 
                                        -- address" (INADDR_ANY for IPv4 addresses, 
                                        -- IN6ADDR_ANY_INIT for IPv6 address). The wildcard 
                                        -- address is used by applications (typically servers) 
                                        -- that intend to accept connections on any of the 
                                        -- hosts's network addresses.
                                        S.AI_PASSIVE,
                                        --  If AI_NUMERICSERV is specified in hints.ai_flags 
                                        -- and service is not NULL, then service must point to 
                                        -- a string containing a numeric port number. This flag
                                        -- is used to inhibit the invocation of a name 
                                        -- resolution service in cases where it is known not to
                                        -- be required.
                                        S.AI_NUMERICSERV
                                ],
                        -- The desired address family for the returned addresses.
                        -- The IPv6Only socket option is set to 0 automatically.
                        -- So that both IPv4 and IPv6 can be handled with one socket.
                        S.addrFamily = S.AF_INET6,
                        -- The preferred socket type.
                        S.addrSocketType = S.Stream,
                        -- Specifies the protocol for the returned socket addresses. 
                        -- Specifying 0 in this field indicates that socket addresses 
                        -- with any protocol can be returned by getaddrinfo().
                        S.addrProtocol = S.defaultProtocol,
                        -- All the other fields in the structure pointed to by hints 
                        -- must contain either 0 or a NULL pointer, as appropriate.
                        S.addrAddress = undefined,
                        S.addrCanonName = undefined
                }
        )
        -- Either node or service, but not both, may be NULL.
        Nothing -- No host.
        -- Port or service.
        (Just $ show portNumber)

--------------------------------------------------------------------------------

destroyServer :: SocketServer -> IO ()
destroyServer socket = S.close (serverSocket socket)

--------------------------------------------------------------------------------

serverPort :: SocketServer -> IO PortNumber
serverPort socket = do
        port <- S.socketPort (serverSocket socket)
        return (toEnum $ fromEnum port)

--------------------------------------------------------------------------------

{-|
        Accept the next client from the socket.
-}
acceptClient :: SocketServer -> IO SocketClient
acceptClient socket = do
        (socket', sockAddr) <- S.accept (serverSocket socket)
        return $ SocketClient socket' sockAddr

{--
        -- To close the Socket after socketToHandle, call hClose on the Handle.
        -- a Handle is automatically closed by a finalizer when it is no longer 
        -- referenced, you should avoid doing any more operations on the Socket 
        -- after calling socketToHandle.
        handle <- S.socketToHandle clientSocket ReadWriteMode
        -- This has the same effect as calling hSetEncoding with char8, 
        -- together with hSetNewlineMode with noNewlineTranslation. 
        hSetBinaryMode handle True
        hSetBuffering handle (BlockBuffering (Just 4096))
        return (handle, sockAddr)
--}

--------------------------------------------------------------------------------

destroyClient :: SocketClient -> IO ()
destroyClient (SocketClient socket _) = do
        S.shutdown socket S.ShutdownBoth
        S.close socket

--------------------------------------------------------------------------------

send :: SocketClient -> BS.ByteString -> IO Int
send (SocketClient socket _) = SBS.send socket

recv :: SocketClient -> Int -> IO BS.ByteString
recv (SocketClient socket _) = SBS.recv socket
