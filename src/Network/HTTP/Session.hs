{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Network.HTTP.Session (
        Storage,
        newStorage,
        newSession,
        getSession,
        getAll,
        setSession,
        deleteSession) where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Char8 as BSS
import System.Random (newStdGen, randomRs)
-- Using only this functions the MVar is atomic because there are no producers.
import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar_, modifyMVar)
import qualified Data.Map as M
import qualified Network.HTTP.Cookie as C

--------------------------------------------------------------------------------

data Storage s = Storage {
        cookieName :: C.Name,
        mutableMap :: MVar (M.Map C.Value s)
}

-- Every storage operation is atomic!
-- To ensure this, every function makes a takeMVar first.
-- No function can putMVar without first doing takeMVar.

-- Create a new session storage.
-- Its only parameter is the session cookie name.
newStorage :: C.Name -> IO (Storage s)
newStorage n = do
        mv <- newMVar (M.empty :: M.Map C.Value s)
        return $ Storage {cookieName = n,  mutableMap = mv}

-- Create a new session cookie and return the header to send to the client.
newSession :: Storage s -> s -> C.Params -> IO BSS.ByteString
newSession (Storage n mv) s cp = do
        modifyMVar mv (newSession' s n cp)

newSession' :: s -> C.Name -> C.Params -> M.Map C.Value s -> IO (M.Map C.Value s, BSS.ByteString)
newSession' s n cp m = do
        stdGen <- newStdGen
        let v = BSS.pack $ take 128 $
                filter (flip notElem [' ','=',';']) $ randomRs ('!', '~') stdGen
        case M.lookup v m of
                -- Already exists.
                Just _ -> newSession' s n cp m
                -- Does not exists. 
                Nothing -> return (M.insert v s m, C.setCookieValue n v cp)

getSession :: Storage s -> BSS.ByteString -> IO (Maybe s)
getSession (Storage n mv) str = do
        case C.getValue n str of
                Nothing -> return Nothing
                Just v -> withMVar mv $ return . (M.lookup v)

getAll :: Storage s -> IO [(BSS.ByteString,s)]
getAll (Storage _ mv) = withMVar mv $ return . M.assocs

-- Set the session of the corresponding request cookie.
-- There is no update function to prevent users from blocking all other requests.
setSession :: Storage s -> s -> BSS.ByteString -> IO ()
setSession (Storage n mv) s bs = do
        case C.getValue n bs of
                Nothing -> return ()
                Just v -> modifyMVar_ mv $ return . (M.adjust (const s) v)

-- Delete the session of the corresponding request cookie.
-- Returns the header that must be sent to the client so it erases the cookie.
deleteSession :: Storage s -> BSS.ByteString -> IO ()
deleteSession (Storage n mv) bs = do
        case C.getValue n bs of
                Nothing -> return ()
                Just v -> modifyMVar_ mv $ return . (M.delete v) 

-- TODO: deleteStorage function??
