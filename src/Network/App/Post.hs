{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Network.App.Post (
        processPostRequest
) where

--------------------------------------------------------------------------------

-- Package: bytestring.
import qualified Data.ByteString as BS
-- Package: aeson.
import qualified Data.Aeson as Aeson

--------------------------------------------------------------------------------

processPostRequest :: (Aeson.FromJSON params, Aeson.ToJSON a)
                   => BS.ByteString
                   -> (params -> IO a)
                   -> IO Aeson.Value
processPostRequest requestBody action = do
        let eitherRequest = (Aeson.eitherDecodeStrict requestBody :: (Aeson.FromJSON params => Either String params))
        case eitherRequest of
                Left err -> error $ show err
                Right params -> do
                        response <- action params
                        return $ Aeson.toJSON response
