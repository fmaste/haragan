{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- On locking:
-- https://blog.2ndquadrant.com/postgresql-anti-patterns-read-modify-write-cycles/
-- https://www.postgresql.org/docs/current/static/explicit-locking.html

--------------------------------------------------------------------------------

module Database.Internal (

        -- * Connect/Disconnect.

          Connection (toPsql)
        , connectPsql
        , closePsql

        -- * Server.

        , getServerId
        , getBizVersionId

        -- * Transactions.

        , beginTransactionRO
        , beginTransactionRWReadCommitted
        , beginTransactionRWSerial
        , commit
        , rollback
        , shouldRetry

        -- * Optimizations.

        , setSeqScanOn
        , setSeqScanOff

        -- * Sharding.

        , createGlobalId

        -- * Business methods.

        , resetBizMethodIds
        , setBizMethodIds
        , getBizMethodIds

        , createBizMethodCall
        , createBizMethodCallWithId
        , createBizMethodCallRetry

        , notifyBizMethodCall

        , getPreviousBizMethodsNotPublished
        , deleteBizMethodsNotPublished

        -- * Data methods.

        , resetBizMethodDataMethodsCounter
        , getBizMethodDataMethodsCounter

        , newBizMethodOperationInsert
        , newBizMethodOperationUpdate
        , newBizMethodOperationDelete

        -- * Replication.

        , replicateDataMethodCall

) where

--------------------------------------------------------------------------------

-- base.
import qualified Control.Concurrent.MVar as MVar
import Data.String (fromString)
import Data.Word (Word32)
import Foreign.C.Types (CUInt (CUInt))
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Char8 as BS8
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as PSQLTrans
import qualified Database.PostgreSQL.Simple.Types as PSQLTypes
-- Package: text.
import qualified Data.Text as Text
-- Package: unordered-containers.
import qualified Data.HashMap.Strict as HM
-- Home Package: haragan.
import qualified Database.Internal.Meta as Meta
import qualified Database.Internal.Catalog as Catalog

--------------------------------------------------------------------------------

data Connection = Connection
        { toPsql :: PSQL.Connection
        , bizMethodIdMVar :: MVar.MVar Integer
        , bizMethodCallIdMVar :: MVar.MVar Integer
        , dataMethodsCounterMVar :: MVar.MVar Integer
        }

-- The business version is not wired at connection time, so during a whole
-- connection it is not assumed to be the same version.
connectPsql :: String -> IO Connection
connectPsql connStr = do
        let connBs = BS8.pack connStr
        psql <- PSQL.connectPostgreSQL connBs
        {-- TODO: Force to always work on transaction mode, no autocommits.
                With this command:
                _ <- PSQL.execute_ psql "SET AUTOCOMMIT TO OFF;"
                Error: "unrecognized configuration parameter \"autocommit\""
        --}
        {-- TODO: Check LC_COLLATE and LC_CTYPE for en_US.UTF-8
                Columns datcollate and datctype on pg_database.
        --}
        {-- TODO: Check timezone --}
        mVarBizMethodId <- MVar.newEmptyMVar
        mVarBizMethodCallId <- MVar.newEmptyMVar
        mVarCounter <- MVar.newMVar 0
        return
                (Connection
                        psql
                        mVarBizMethodId
                        mVarBizMethodCallId
                        mVarCounter
                )

closePsql :: Connection -> IO ()
closePsql conn = PSQL.close (toPsql conn)

-- Server.
--------------------------------------------------------------------------------

-- Rarely updated, SELECT without serialization concerns.
getServerId :: Connection -> IO Integer
getServerId conn = do
        -- TODO: Still using "shard" instead of "server" in the database.
        [(PSQL.Only shardId)] <- PSQL.query_ (toPsql conn)
                "SELECT public.shard_id()::BIGINT;"
        return shardId

-- Rarely updated, SELECT without serialization concerns.
getBizVersionId :: Connection -> [Integer] -> IO Integer
getBizVersionId conn version = do
         bizVersionId <- Catalog.getBizVersionId (toPsql conn) version
         return bizVersionId

--------------------------------------------------------------------------------

createGlobalId :: Connection -> IO Integer
createGlobalId conn = do
        [(PSQL.Only globalId)] <- PSQL.query_ (toPsql conn)
                "SELECT public.global_id_generator()::BIGINT;"
        return globalId

--------------------------------------------------------------------------------

transactionModeROSerial :: PSQLTrans.TransactionMode
transactionModeROSerial =
        (PSQLTrans.TransactionMode
                PSQLTrans.Serializable
                PSQLTrans.ReadOnly
        )

transactionModeRWReadCommitted :: PSQLTrans.TransactionMode
transactionModeRWReadCommitted =
        (PSQLTrans.TransactionMode
                PSQLTrans.ReadCommitted
                PSQLTrans.ReadWrite
        )

transactionModeRWSerial :: PSQLTrans.TransactionMode
transactionModeRWSerial =
        (PSQLTrans.TransactionMode
                PSQLTrans.Serializable
                PSQLTrans.ReadWrite
        )

beginTransactionRO :: Connection -> IO ()
beginTransactionRO = beginTransactionROSerial

beginTransactionROSerial :: Connection -> IO ()
beginTransactionROSerial conn = PSQLTrans.beginMode
        transactionModeROSerial
        (toPsql conn)

beginTransactionRWReadCommitted :: Connection -> IO ()
beginTransactionRWReadCommitted conn = PSQLTrans.beginMode
        transactionModeRWReadCommitted
        (toPsql conn)

beginTransactionRWSerial :: Connection -> IO ()
beginTransactionRWSerial conn = PSQLTrans.beginMode
        transactionModeRWSerial
        (toPsql conn)

commit :: Connection -> IO ()
commit conn = PSQLTrans.commit (toPsql conn)

rollback :: Connection -> IO ()
rollback conn = PSQLTrans.rollback (toPsql conn)

-- | Any transaction which must be rolled back to prevent serialization
-- anomalies will fail with SQLSTATE 40001, which has a standard meaning of
-- "serialization failure".
shouldRetry :: Maybe PSQL.SqlError -> Bool
shouldRetry Nothing = False
shouldRetry (Just sqlErr) =
        -- Retry only on transaction related errors!
        let error1 = PSQLTrans.isSerializationError sqlErr
            error2 = PSQLTrans.isNoActiveTransactionError sqlErr
            error3 = PSQLTrans.isFailedTransactionError sqlErr
        in if error1 || error2 || error3
                then True
                else False

--------------------------------------------------------------------------------

{--
import Control.Exception
import Data.Dynamic (Dynamic)
import Data.Void (Void)
import System.Exit (ExitCode)

-- On non async or heap/memory exceptions rollback savepoint and continue.
withSavepointCatch :: Connection -> IO a -> IO (Either SomeException a)
withSavepointCatch conn action = catch
        (do
                ans <- PSQL.withSavepoint (toPsql conn) action
                return (Right ans)
        )
        (\e ->
                if shouldCatch (e :: SomeException)
                        then do
                                print $ displayException e
                                return (Left e)
                        else throwIO e
        )

-- What exceptions to catch. Ordered from the instances list of:
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Exception.html#t:Exception
shouldCatch :: SomeException -> Bool
shouldCatch e
        -- Falses.
        -- Async or memory/heap errors.
        | Just (_ :: AsyncException) <- fromException e = False
        | Just (_ :: SomeAsyncException) <- fromException e = False
        | Just (_ :: AllocationLimitExceeded) <- fromException e = False
        -- Trues.
        -- postgresql-simple.
        | Just (_ :: PSQL.SqlError) <- fromException e = True
        | Just (_ :: PSQL.QueryError) <- fromException e = True
        | Just (_ :: PSQL.FormatError) <- fromException e = True
        -- All the others.
        | Just (_ :: ArithException) <- fromException e = True
        | Just (_ :: ErrorCall) <- fromException e = True
        | Just (_ :: Dynamic) <- fromException e = True
        | Just (_ :: IOException) <- fromException e = True
        | Just (_ :: ExitCode) <- fromException e = True
        | Just (_ :: ArrayException) <- fromException e = True
        | Just (_ :: AssertionFailed) <- fromException e = True
        | Just (_ :: Deadlock) <- fromException e = True
        | Just (_ :: BlockedIndefinitelyOnSTM) <- fromException e = True
        | Just (_ :: BlockedIndefinitelyOnMVar) <- fromException e = True
        | Just (_ :: NestedAtomically) <- fromException e = True
        | Just (_ :: NonTermination) <- fromException e = True
        | Just (_ :: TypeError) <- fromException e = True
        | Just (_ :: NoMethodError) <- fromException e = True
        | Just (_ :: RecUpdError) <- fromException e = True
        | Just (_ :: RecConError) <- fromException e = True
        | Just (_ :: RecSelError) <- fromException e = True
        | Just (_ :: PatternMatchFail) <- fromException e = True
        | Just (_ :: Void) <- fromException e = True
        -- Default.
        | otherwise = True
--}

--------------------------------------------------------------------------------

-- | Enables the query planner's use of sequential scan plan types. It is
-- impossible to suppress sequential scans entirely, but turning this variable
-- off discourages the planner from using one if there are other methods
-- available. The default is on.
setSeqScanOn :: Connection -> IO ()
setSeqScanOn conn = do
        -- The effects of SET LOCAL last only till the end of the current SQL
        -- transaction, whether committed or not.
        -- On abort or commit this value is lost.
        -- The effects of SET or SET LOCAL are also canceled by rolling back to
        -- a savepoint that is earlier than the command.
        _ <- PSQL.execute_ (toPsql conn) "SET LOCAL enable_seqscan = on;"
        return ()

-- | Disables the query planner's use of sequential scan plan types. It is
-- impossible to suppress sequential scans entirely, but turning this variable
-- off discourages the planner from using one if there are other methods
-- available. The default is on.
setSeqScanOff :: Connection -> IO ()
setSeqScanOff conn = do
        -- The effects of SET LOCAL last only till the end of the current SQL
        -- transaction, whether committed or not.
        -- On abort or commit this value is lost.
        -- The effects of SET or SET LOCAL are also canceled by rolling back to
        -- a savepoint that is earlier than the command.
        _ <- PSQL.execute_ (toPsql conn) "SET LOCAL enable_seqscan = off;"
        return ()

-- Transaction-bounded variable with the business method ID.
--------------------------------------------------------------------------------

{--

resetBizMethodId :: PSQL.Connection -> IO ()
resetBizMethodId psql = do
        -- Fist reset the parameter that stores the current transaction ID.
        -- RESET restores run-time parameters to their default values. RESET is
        -- an alternative spelling for "SET configuration_parameter TO DEFAULT".
        -- The default value is defined as the value that the parameter would
        -- have had, if no SET had ever been issued for it in the current
        -- session.
        _ <- PSQL.execute_ psql "RESET intr.current_business_method_id;"
        return ()

setBizMethodId :: PSQL.Connection -> Integer -> IO ()
setBizMethodId psql methodId = do
        -- Store the transaction ID but bounded to the current SQL transaction.
        -- The effects of SET LOCAL last only till the end of the current SQL
        -- transaction, whether committed or not.
        -- On abort or commit this value is lost.
        -- The effects of SET or SET LOCAL are also canceled by rolling back to
        -- a savepoint that is earlier than the command.
        _ <- PSQL.execute psql
                "SET LOCAL intr.current_business_method_id TO ?;"
                (PSQL.Only methodId)
        return ()

-- If there is no setting named like the param,
-- current_setting throws an error unless
-- missing_ok is supplied and is true.
-- If the setting exists but not the method also
-- fails due to a REFERENCES constraint.
<> " , current_setting( "
        <> "'intr.current_business_method_id'"
<> " )::BIGINT "

--}

--------------------------------------------------------------------------------

-- | Reset the actual business method no matter in what state is the connection.
resetBizMethodIds :: Connection -> IO ()
resetBizMethodIds conn = do
        _ <- MVar.tryTakeMVar (bizMethodIdMVar conn)
        _ <- MVar.tryTakeMVar (bizMethodCallIdMVar conn)
        return ()

-- | Sets the actual busines method call on the connection. When finished
-- 'resetBizMethodIds' must always be called.
-- This functions blocks if the business method was already set. Use
-- 'resetBizMethodIds' first.
setBizMethodIds :: Connection -> (Integer,Integer) -> IO ()
setBizMethodIds conn (bizMethodId, bizMethodCallId) = do
        MVar.putMVar (bizMethodIdMVar conn) bizMethodId
        MVar.putMVar (bizMethodCallIdMVar conn) bizMethodCallId

-- | Get the actual busines method call of the connection.
-- This functions blocks if the business method was not already set. Use
-- 'setBizMethodIds' first.
getBizMethodIds :: Connection -> IO (Integer,Integer)
getBizMethodIds conn = do
        bizMethodId <- MVar.readMVar (bizMethodIdMVar conn)
        bizMethodCallId <- MVar.readMVar (bizMethodCallIdMVar conn)
        return (bizMethodId, bizMethodCallId)

-- Creating and storing a business method.
--------------------------------------------------------------------------------

createBizMethodCall :: Connection
                    -> String
                    -> [Integer]
                    -> IO (Integer,Integer)
createBizMethodCall conn name version = do
        -- The business method name must exists.
        bizMethodId <- Catalog.getBizMethodId
                (toPsql conn) name
        -- Now create the business method call.
        bizMethodCallId <- Meta.newBizMethodCall
                (toPsql conn)
        -- Bye!
        return (bizMethodId, bizMethodCallId)

-- | Same as 'createBizMethodCall' but the ID of the call can be provided.
createBizMethodCallWithId :: Connection
                          -> String
                          -> [Integer]
                          -> Integer
                          -> IO (Integer,Integer)
createBizMethodCallWithId conn name version bizMethodCallId = do
        -- The business method name must exists.
        bizMethodId <- Catalog.getBizMethodId
                (toPsql conn) name
        -- Now create the business method call.
        _ <- Meta.newBizMethodCallWithId
                (toPsql conn) bizMethodCallId
        -- Bye!
        return (bizMethodId, bizMethodCallId)

createBizMethodCallRetry :: Connection -> String -> Integer -> IO Integer
createBizMethodCallRetry conn errorMsg bizMethodCallId = do
        bizMethodCallRetryId <- Meta.newBizMethodCallRetry
                (toPsql conn) bizMethodCallId errorMsg
        return bizMethodCallRetryId

--------------------------------------------------------------------------------

-- | Notifies the business methods call using PostgreSQL NOTIFY command.
-- The name of the notification is the business name and the payload is the
-- business method call ID.
notifyBizMethodCall :: Connection -> IO ()
notifyBizMethodCall conn = do
        (bizMethodId, bizMethodCallId) <- getBizMethodIds conn
        [(PSQL.Only (bizMethodName::String))] <- PSQL.query (toPsql conn)
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT display_name "
                        <> " FROM public.business_methods "
                        <> " WHERE id = ? "
                )
                (PSQL.Only bizMethodId)
        _ <- PSQL.execute_ (toPsql conn)
                (fromString $ " NOTIFY "
                        ++ "   " ++ (show bizMethodName)
                        ++ " , '" ++ (show bizMethodCallId) ++ "' "
                        ++ " ; "
                )
        return ()

--------------------------------------------------------------------------------

-- | Business method that updates regulary may want to have its previous runs
-- deleted if they are not replicated to avoid wasting extra space and time.
-- Think in a counter were its previous values are not relevant, we just want
-- to replicate the last one.
getPreviousBizMethodsNotPublished :: Connection
                                  -> Integer
                                  -> Integer
                                  -> (Int,Int)
                                  -> IO [Integer]
getPreviousBizMethodsNotPublished conn bizMethodId bizMethodCallId (limit,offset) = do
        (ans :: [PSQL.Only Integer]) <- PSQL.query (toPsql conn)
                (" SELECT id "
                        <> " FROM public.business_methods_calls "
                        <> " WHERE "
                                <> " NOT id = ? "
                                <> " AND "
                                <> " business_methods_id = ? "
                                <> " AND "
                                <> " business_versions_id = ( "
                                        <> " SELECT business_versions_id "
                                        <> " FROM business_methods_calls "
                                        <> " WHERE id = ? "
                                <> ") "
                                <> " AND "
                                <> " schema_versions_id = ( "
                                        <> " SELECT schema_versions_id "
                                        <> " FROM business_methods_calls "
                                        <> " WHERE id = ? "
                                <> ") "
                                <> " AND "
                                <> " origin_shard_id = ( "
                                        <> " SELECT origin_shard_id "
                                        <> " FROM business_methods_calls "
                                        <> " WHERE id = ? "
                                <> ") "
                                <> " AND "
                                <> " id NOT IN ( "
                                        <> " SELECT business_methods_calls_id "
                                        <> " FROM public.subscriptions_publications "
                                <> " ) "
                        <> " ORDER BY creation_time ASC "
                        <> " LIMIT ? "
                        <> " OFFSET ? "
                        <> " ; "
                )
                (
                          bizMethodCallId
                        , bizMethodId
                        , bizMethodCallId
                        , bizMethodCallId
                        , bizMethodCallId
                        , limit
                        , offset
                )
        return $ map PSQL.fromOnly ans

deleteBizMethodsNotPublished :: Connection -> [Integer] -> IO ()
deleteBizMethodsNotPublished _ [] = return ()
deleteBizMethodsNotPublished conn bizMethodIds = do
        _ <- PSQL.execute (toPsql conn)
                ("DELETE FROM "
                        <> " public.data_methods_calls "
                        <> " WHERE business_methods_calls_id IN ? "
                        <> " ; "
                )
                (PSQL.Only $ PSQL.In bizMethodIds)
        _ <- PSQL.execute (toPsql conn)
                ("DELETE FROM "
                        <> " public.business_methods_calls "
                        <> " WHERE id IN ? "
                        <> " ; "
                )
                (PSQL.Only $ PSQL.In bizMethodIds)
        return ()

--------------------------------------------------------------------------------

resetBizMethodDataMethodsCounter :: Connection -> IO ()
resetBizMethodDataMethodsCounter conn = do
        _ <- MVar.swapMVar (dataMethodsCounterMVar conn) 0
        return ()

getBizMethodDataMethodsCounter :: Connection -> IO Integer
getBizMethodDataMethodsCounter conn = do
        MVar.readMVar (dataMethodsCounterMVar conn)

--------------------------------------------------------------------------------

-- | How to convert from OIDs. Currently not needed.
_fromOid :: PSQLTypes.Oid -> Word32
_fromOid (PSQLTypes.Oid (CUInt w32)) = w32

-- | How to convert to OIDs. Currently not needed.
_toOid :: Word32 -> PSQLTypes.Oid
_toOid w32 = (PSQLTypes.Oid (CUInt w32))

-- Given a schema name and a table name return its OIDs by looking on the system
-- catalog. An error occurs if the qualified table name does not exists.
checkOids :: PSQL.Connection
          -> String
          -> String
          -> IO (PSQLTypes.Oid, PSQLTypes.Oid)
checkOids psql psSchemaName tableName = do
        [(PSQL.Only schemaOid)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT oid "
                        <> " FROM pg_namespace "
                        <> " WHERE nspname = ? "
                        <> " ; "
                )
                (PSQL.Only psSchemaName)
        [(PSQL.Only tableOid)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT oid "
                        <> " FROM pg_class "
                        <> " WHERE "
                                <> " relnamespace = ? "
                                <> " AND "
                                <> " relname = ? "

                        <> " ; "
                )
                (schemaOid, tableName)
        return (schemaOid, tableOid)

-- Store the newly created row as a JSON.
-- If a Business method is not already created this function fails with an SQL
-- error. Same for the existance of the schema name, table name or row named ID
-- with its value being the supplied number.
newBizMethodOperationInsert :: Connection
                            -> String
                            -> String
                            -> [Integer]
                            -> String
                            -> Integer
                            -> IO Integer
newBizMethodOperationInsert conn schema name version table rowId = do
        let psql = toPsql conn
        -- The bizMethodIdMVar must be full.
        bizMethodId <- MVar.readMVar (bizMethodIdMVar conn)
        -- The business version must exists.
        bizVersionId <- Catalog.getDataMethod psql version
        -- The schema-table must exists.
        _ <- checkOids psql schema table
        -- The data method name must exists for this module, table, verb and
        -- version.
        dataMethodId <- Catalog.getDataMethod
                psql schema name table Catalog.INSERT
        -- The data version for this business version must exist.
        methodVersionId <- Catalog.getMethod psql
                bizMethodId bizVersionId dataMethodId
        -- The bizMethodCallIdMVar must be full.
        bizMethodCallId <- MVar.readMVar (bizMethodCallIdMVar conn)
        -- Insert the data method call.
        transOpId <- Meta.newDataMethodInsert psql schema table rowId
        -- Insert the biz method data method.
        _ <- Meta.newBizMethodCallDataMethodCall psql
                methodVersionId bizMethodCallId transOpId
        -- Plus 1 data method.
        MVar.modifyMVar_
                (dataMethodsCounterMVar conn)
                (\counter -> return $ counter + 1)
        return transOpId

-- Store the newly updated row as a JSON.
-- If a Business method is not already created this function fails with an SQL
-- error. Same for the existance of the schema name, table name or row named ID
-- with its value being the supplied number.
newBizMethodOperationUpdate :: Connection
                            -> String
                            -> String
                            -> [Integer]
                            -> String
                            -> Integer
                            -> IO Integer
newBizMethodOperationUpdate conn schema name version table rowId = do
        let psql = toPsql conn
        -- The bizMethodIdMVar must be full.
        bizMethodId <- MVar.readMVar (bizMethodIdMVar conn)
        -- The business version must exists.
        bizVersionId <- Catalog.getDataMethod psql version
        -- The schema-table must exists.
        _ <- checkOids psql schema table
        -- The data method name must exists for this module, table, verb and
        -- version.
        dataMethodId <- Catalog.getDataMethod
                psql schema name table Catalog.INSERT
        -- The data version for this business version must exist.
        methodVersionId <- Catalog.getMethod psql
                bizMethodId bizVersionId dataMethodId
        -- The bizMethodCallIdMVar must be full.
        bizMethodCallId <- MVar.readMVar (bizMethodCallIdMVar conn)
        -- Insert the data method call.
        transOpId <- Meta.newDataMethodUpdate psql schema table rowId
        -- Insert the biz method data method.
        _ <- Meta.newBizMethodCallDataMethodCall psql
                methodVersionId bizMethodCallId transOpId
        -- Plus 1 data method.
        MVar.modifyMVar_
                (dataMethodsCounterMVar conn)
                (\counter -> return $ counter + 1)
        return transOpId

-- Store the newly deleted action.
-- If a Business method is not already created this function fails with an SQL
-- error. Same for the existance of the schema name, table name or row named ID
-- with its value being the supplied number.
newBizMethodOperationDelete :: Connection
                            -> String
                            -> String
                            -> [Integer]
                            -> String
                            -> Integer
                            -> IO Integer
newBizMethodOperationDelete conn schema name version table rowId = do
        let psql = toPsql conn
        -- The bizMethodIdMVar must be full.
        bizMethodId <- MVar.readMVar (bizMethodIdMVar conn)
        -- The business version must exists.
        bizVersionId <- Catalog.getDataMethod psql version
        -- The schema-table must exists.
        _ <- checkOids psql schema table
        -- The data method name must exists for this module, table, verb and
        -- version.
        dataMethodId <- Catalog.getDataMethod
                psql schema name table Catalog.INSERT
        -- The data version for this business version must exist.
        methodVersionId <- Catalog.getMethod psql
                bizMethodId bizVersionId dataMethodId
        -- The bizMethodCallIdMVar must be full.
        bizMethodCallId <- MVar.readMVar (bizMethodCallIdMVar conn)
        -- Insert the data method call.
        transOpId <- Meta.newDataMethodDelete psql rowId
        -- Insert the biz method data method.
        _ <- Meta.newBizMethodCallDataMethodCall psql
                methodVersionId bizMethodCallId transOpId
        -- Plus 1 data method.
        MVar.modifyMVar_
                (dataMethodsCounterMVar conn)
                (\counter -> return $ counter + 1)
        return transOpId

--------------------------------------------------------------------------------

replicateDataMethodCall :: Connection
                        -> Integer -- ^ Data method call ID.
                        -> String -- ^ SQL action verb.
                        -> (Text.Text, Text.Text) -- ^ Schema and table name.
                        -> (Maybe Integer) -- ^ Row ID.
                        -> (Maybe Aeson.Value) -- ^ Row data.
                        -> IO ()
replicateDataMethodCall conn dmcId "INSERT" (schema, table) _ _ = do
        let tableName = (Text.unpack schema) <> "." <> (Text.unpack table)
        _ <- PSQL.execute (toPsql conn)
                {--
                INSERT INTO
                        TABLE_GROUP.TABLE_NAME
                SELECT * FROM
                        jsonb_populate_record(
                                NULL::TABLE_GROUP.TABLE_NAME
                                , (
                                        SELECT row_data FROM
                                        public.data_methods_calls
                                        WHERE id = DataMethodCallId
                                  )
                        )
                ;
                --}
                (fromString $ " INSERT INTO "
                        <> tableName
                        <> " SELECT * FROM "
                        <> " jsonb_populate_record( "
                                        <> " NULL::" <> tableName
                                <> " , "
                                        <> " ( "
                                                <> " SELECT row_data FROM "
                                                <> " public.data_methods_calls "
                                                <> " WHERE id = ? "
                                        <> " ) "
                        <> " ) "
                        <> " ; "
                )
                (PSQL.Only dmcId)
        return ()
replicateDataMethodCall conn dmcId "UPDATE" (schema, table) (Just rId) (Just (Aeson.Object obj)) = do
        let tableName = (Text.unpack schema) <> "." <> (Text.unpack table)
        _ <- PSQL.execute (toPsql conn)
                {--
                UPDATE TABLE_GROUP.TABLE_NAME
                SET FIELD1 = subquery
                FROM (SELECT * FROM
                        jsonb_populate_record(
                                NULL::TABLE_GROUP.TABLE_NAME
                                , (
                                        SELECT data FROM
                                        public.data_methods_calls
                                        WHERE
                                        id = SchemaActionId
                                  )
                        )
                ) AS subquery
                ;
                --}
                (fromString $
                           " WITH row_data AS ( "
                                <> " SELECT * FROM "
                                <> " jsonb_populate_record( "
                                                <> " NULL::" <> tableName
                                        <> " , "
                                                <> " ( "
                                                <> " SELECT row_data FROM "
                                                <> " public.data_methods_calls "
                                                <> " WHERE id = ? "
                                                <> " ) "
                                <> " ) "
                        <> " ) "
                        <> " UPDATE "
                                <> tableName <> " AS to_update "
                        <> " SET "
                                <> (setRows obj "row_data")
                        <> " FROM "
                                <> " row_data "
                        <> " WHERE "
                                <> " to_update.id = ? "
                        <> " ; "
                )
                (dmcId, rId)
        return ()
replicateDataMethodCall conn _ "DELETE" (schema, table) (Just rowId) _ = do
        let tableName = (Text.unpack schema) <> "." <> (Text.unpack table)
        _ <- PSQL.execute (toPsql conn)
                (fromString $ " DELETE FROM "
                        <> tableName
                        <> " WHERE id = ? "
                        <> " ; "
                )
                (PSQL.Only rowId)
        return ()
replicateDataMethodCall _ _ _ _ _ _ = do
        error "<1>replicateDataMethodCall"

setRows :: Aeson.Object -> Text.Text -> String
setRows obj subqueryName = Text.unpack $ Text.intercalate " , " $ map
        (\k ->  k <> " = " <> subqueryName <> "." <> k)
        (HM.keys obj)
