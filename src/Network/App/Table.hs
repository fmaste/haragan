{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Network.App.Table (
          Request (..)
        , RequestPagination (..)
        , RequestSearch (..)
        , RequestColumn (..) 
        , RequestOrder (..)
        , Response (..) 
        , ResponseRow (..)
        , processTableRequest
        , processTableRequest'
) where

--------------------------------------------------------------------------------

import qualified GHC.Generics as GG
-- Package: bytestring.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Package: text.
import qualified Data.Text as Text
-- Package containers.
import qualified Data.Map as Map
-- Package: aeson.
import qualified Data.Aeson as Aeson

--------------------------------------------------------------------------------

data Request filters = Request {
          tableFilters :: Maybe filters
        , tableSearch :: Maybe RequestSearch
        , tablePagination :: Maybe RequestPagination
        , tableColumns :: Map.Map String RequestColumn
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON filters => Aeson.FromJSON (Request filters)

data RequestPagination = RequestPagination {
        -- | Paging first record indicator. This is the start point in the 
        -- current data set (0 index based - i.e. 0 is the first record).
          paginationFirstRecord :: Int
        -- | Number of records that the table can display in the current draw. 
        -- It is expected that the number of records returned will be equal to 
        -- this number, unless the server has fewer records to return.
        , paginationMaxRecords :: Int
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON RequestPagination

data RequestSearch = RequestSearch {
        -- | Global search value. To be applied to the entire table.
        searchString :: Text.Text
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON RequestSearch

data RequestColumn = RequestColumn {
          columnSearch :: Maybe RequestSearch
        , columnOrder :: Maybe RequestOrder
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON RequestColumn

data RequestOrder = RequestOrder {
        -- | Ordering direction for this column. It will be asc or desc to 
        -- indicate ascending ordering or descending ordering, respectively.
        orderDirection :: String
} deriving (Show, Eq, GG.Generic)

instance Aeson.FromJSON RequestOrder

data Response a = Response {
        -- | Total records, before filtering (i.e. the total number of records 
        -- in the database).
          recordsTotal :: Int
        -- | Total records, after filtering (i.e. the total number of records 
        -- after filtering has been applied - not just the number of records 
        -- being returned for this page of data).
        , recordsFiltered :: Int
        -- | The data to be displayed in the table. This is an array of data 
        -- source objects, one for each row.
        , tableRows :: [ ResponseRow a ]
} deriving (Show, Eq, GG.Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (Response a)

data ResponseRow a = ResponseRow {
          rowId :: Aeson.Value
        , rowData :: a
} deriving (Show, Eq, GG.Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (ResponseRow a)

--------------------------------------------------------------------------------

processTableRequest :: (Aeson.FromJSON filters, Aeson.ToJSON a) => BS.ByteString -> ((Request filters) -> IO (Response a)) -> IO BS.ByteString
processTableRequest requestBody action = do
        let eitherRequest = (Aeson.eitherDecodeStrict requestBody :: (Aeson.FromJSON filters => Either String (Request filters)))
        case eitherRequest of
                Left err -> error $ show err
                Right request -> do
                        response <- action request
                        return $ BSL.toStrict $ Aeson.encode response

--------------------------------------------------------------------------------

processTableRequest' :: (Aeson.FromJSON filters, Aeson.ToJSON a) => BS.ByteString -> (Maybe filters -> Maybe Text.Text -> Maybe (Int,Int) -> IO (Response a)) -> IO BS.ByteString
processTableRequest' requestBody action = processTableRequest requestBody
        (\request -> do
                let maybeFilters = tableFilters request
                let maybeSearch = case tableSearch request of
                        Nothing -> Nothing
                        Just requestSearch ->
                                let text = Text.strip (searchString requestSearch)
                                in if text == ""
                                        then Nothing
                                        else Just text
                let maybePagination = case tablePagination request of
                        Nothing -> Nothing
                        Just (RequestPagination offset limit) ->
                            Just (limit, offset)
                response <- action maybeFilters maybeSearch maybePagination
                return response
        )
