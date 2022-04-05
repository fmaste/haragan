{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Use to create Business Method Calls on the metadata (Schema META) (not to be
-- confused with the Business Methods of the Catalog (Schema CATALOG, on
-- "Database.Internal.Catalog")).
--
-- Here we don't check if the business method call is available on the current
-- catalog/version. This is an internal module with little responsabilities.
-- Same when storing a Data Method Call, not checking if the method exists.
--
-- When finished with a Data Method Call 'newBizMethodCallDataMethodCall' is
-- used to relate it to its parent Business Method Call and the Catalog.
--
-- If you forgot to call this last function scary things may happen!

module Database.Internal.Meta (

        -- * Business Methods Calls.

          newBizMethodCall
        , newBizMethodCallWithId
        , newBizMethodCallRetry

        -- * Data Methods Calls.

        , newDataMethodInsert
        , newDataMethodUpdate
        , newDataMethodDelete

        -- * Catalog - Business Methods Calls - Data Methods Calls.

        , newBizMethodCallDataMethodCall

) where

--------------------------------------------------------------------------------

-- base.
import Data.String (fromString)
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL

-- Creating and storing a Business Method Call.
--------------------------------------------------------------------------------

-- | The start of a new Business Method Call.
newBizMethodCall :: PSQL.Connection -> IO Integer
newBizMethodCall psql = do
        [(PSQL.Only bizMethodCallId)] <- PSQL.query_ psql
                (" INSERT INTO meta.business_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                        <> " ) "
                        <> " VALUES ( "
                                <> "   DEFAULT "
                                <> " , DEFAULT "
                                <> " , DEFAULT "
                        <> " ) "
                        <> " RETURNING id "
                        <> " ; "
                )
        return bizMethodCallId

-- | Same as 'createBizMethodCall' but the ID of the call can be provided.
newBizMethodCallWithId :: PSQL.Connection -> Integer -> IO ()
newBizMethodCallWithId psql bizMethodCallId = do
        -- Now create the business method call.
        _ <- PSQL.execute psql
                (" INSERT INTO meta.business_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                        <> " ) "
                        <> " VALUES ( "
                                <> "   ? "
                                <> " , DEFAULT "
                                <> " , DEFAULT "
                        <> " ) "
                        <> " ; "
                )
                (PSQL.Only bizMethodCallId)
        return ()

-- Table public.business_methods_calls_retries must use statement_timestamp()
-- as its default value of creation_time.
newBizMethodCallRetry :: PSQL.Connection -> Integer -> String -> IO Integer
newBizMethodCallRetry psql bizMethodCallId errorMsg = do
        [(PSQL.Only bizMethodCallRetryId)] <- PSQL.query psql
                (" INSERT INTO meta.business_methods_calls_retries "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , business_methods_calls_id "
                        <> " ) "
                        <> " VALUES ( "
                                <> "   DEFAULT "
                                <> " , DEFAULT "
                                <> " , ? "
                                <> " , ? "
                        <> " ) "
                        <> " RETURNING id ; "
                )
                (errorMsg, bizMethodCallId)
        return bizMethodCallRetryId

-- Creating and storing a Data Method Call.
--------------------------------------------------------------------------------

-- The business method call ID is not stored here.
-- A call to 'newBizMethodCallDataMethodCall' is needed before COMMIT.
newDataMethodInsert :: PSQL.Connection
                    -> String
                    -> String
                    -> Integer
                    -> IO Integer
newDataMethodInsert psql schema table rowId = do
        -- Insert the data method call.
        [(PSQL.Only (transOpId::Integer))] <- PSQL.query psql
                (fromString $ " INSERT INTO meta.data_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , row_id "
                                <> " , row_data "
                        <> " ) "
                        <> " VALUES "
                        <> " ( "
                                <> "   DEFAULT "
                                <> " , DEFAULT "
                                <> " , DEFAULT "
                                -- Row to insert.
                                <> " , ? "
                                -- Row data as a JSON object.
                                <> " , (SELECT row_to_json(t) "
                                        <> " FROM "
                                        <> schema ++ "." ++ table ++ " AS t "
                                        <> " WHERE id = ? "
                                <> " )"
                        <> " ) "
                        <> " RETURNING id "
                        <> " ; "
                )
                (rowId, rowId)
        return transOpId

-- The business method call ID is not stored here.
-- A call to 'newBizMethodCallDataMethodCall' is needed before COMMIT.
newDataMethodUpdate :: PSQL.Connection
                    -> String
                    -> String
                    -> Integer
                    -> IO Integer
newDataMethodUpdate psql schema table rowId = do
        -- Insert the data method call.
        [(PSQL.Only (transOpId::Integer))] <- PSQL.query psql
                (fromString $ " INSERT INTO meta.data_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , row_id "
                                <> " , row_data "
                        <> " ) "
                        <> " VALUES "
                        <> " ( "
                                <> "   DEFAULT "
                                <> " , DEFAULT "
                                <> " , DEFAULT "
                                -- Row to update.
                                <> " , ? "
                                -- Row data as a JSON object.
                                <> " , (SELECT row_to_json(t) "
                                        <> " FROM "
                                        <> schema ++ "." ++ table ++ " AS t "
                                        <> " WHERE id = ? "
                                <> " )"
                        <> " ) "
                        <> " RETURNING id "
                        <> " ; "
                )
                (rowId, rowId)
        return transOpId

-- The business method call ID is not stored here.
-- A call to 'newBizMethodCallDataMethodCall' is needed before COMMIT.
newDataMethodDelete :: PSQL.Connection
                    -> Integer
                    -> IO Integer
newDataMethodDelete psql rowId = do
        -- Insert the data method call.
        [(PSQL.Only (transOpId::Integer))] <- PSQL.query psql
                (fromString $ " INSERT INTO meta.data_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , row_id "
                                <> " , row_data "
                        <> " ) "
                        <> " VALUES "
                        <> " ( "
                                <> "   DEFAULT "
                                <> " , DEFAULT "
                                <> " , DEFAULT "
                                -- Row to delete.
                                <> " , ? "
                                -- Has no data.
                                <> " NULL "
                        <> " ) "
                        <> " RETURNING id "
                        <> " ; "
                )
                (PSQL.Only rowId)
        return transOpId

--------------------------------------------------------------------------------

-- | Merge all together: the catalog versions, the business method call and the
-- data method calls.
newBizMethodCallDataMethodCall :: PSQL.Connection
                               -> Integer
                               -> Integer
                               -> Integer
                               -> IO Integer
newBizMethodCallDataMethodCall
        psql
        methodId
        bizMethodCallId
        dataMethodCallId = do
                [(PSQL.Only (ans::Integer))] <- PSQL.query psql
                        (fromString $
                                   " INSERT INTO meta.methods_calls "
                                <> " ( "
                                        <> "   id "
                                        <> " , creation_time "
                                        <> " , display_name "
                                        <> " , methods_id "
                                        <> " , business_methods_calls_id "
                                        <> " , data_methods_calls_id "
                                        <> " ) "
                                        <> " VALUES "
                                        <> " ( "
                                        <> "   DEFAULT "
                                        <> " , DEFAULT "
                                        <> " , DEFAULT "
                                        <> " , ? "
                                        <> " , ? "
                                        <> " , ? "
                                <> " ) "
                                <> " RETURNING id "
                                <> " ; "
                        )
                        (methodId, bizMethodCallId, dataMethodCallId)
                return ans
