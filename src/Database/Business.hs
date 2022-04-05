{-# LANGUAGE OverloadedStrings #-}

-- The recommended approach is to separate the data access logic from the
-- business layer (and presentation). This separate layer is referred to as the
-- Data Access Layer, or DAL for short. The benefits of this layered
-- architecture is well documented.
-- All code that is specific to the underlying data source (such as creating a
-- connection to the database, issuing SELECT, INSERT, UPDATE, and DELETE
-- commands, and so on) should be located in the DAL. The other layers should
-- not contain any references to such data access code, but should instead make
-- calls into the DAL for any and all data requests. Data Access Layers
-- typically contain methods for accessing the underlying database data. For
-- example a database that has Products and Categories tables that record the
-- products for sale and the categories to which they belong should have methods
-- like GetCategories() or GetProductsByCategoryID(categoryID).
--
-- Here we separate the data layer into different data methods and is up to the
-- business logic to decide how to use this functions of or how to relate the
-- different DAL function's.
-- 
-- Every data method is the minimal access we desire to have on the database.
-- Its only responsibility is to log its operations right before they are done.
-- It up to the business logic to create the SQL transaction.

--------------------------------------------------------------------------------

module Database.Business (

        -- * Connect/Disconnect.

          Connection ()
        , connectRO, connectRW
        , close

        -- * Server.

        , getServerId

        -- * Transactions.

        -- ** RO.

        , withTransactionRO

        -- ** RW.

        , withBizMethod, withBizMethodAsync, withBizMethodReuse

        -- ** Optimizations.

        , withEncourageIndexScan

        -- * Subscriptions and Replications.

        , publishBizMethods
        , replicateBizMethods

) where

--------------------------------------------------------------------------------

-- base.
import Control.Monad (when)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception (
          SomeException
        , fromException
        , throwIO
        , handle
        , try
        , mask
        , finally)
import Data.Either (either)
-- Package: async.
import qualified Control.Concurrent.Async as Async
-- Home Package: haragan.
import qualified Database.Internal as Internal
import qualified Database.Schema.Subscriptions as Subs

--------------------------------------------------------------------------------

{--

A Connection object to help avoid this two problems:
1) Every block of statements always represents a Business Method and is done
isolated either on an RO or RW serializable transaction.
TODO: These business method need to be ran on isolation, no partial output
should be used by anyone, because the action may be retried. Create a special
monad, like the ST monad of the postgresql-transactional package.
2) When reusing a connection only one Business Method at a time can run. This is
to avoid concurrent uses of the session that hold the open transaction.

From PostgreSQL docs:
(https://www.postgresql.org/docs/current/static/sql-begin.html)
Issuing BEGIN when already inside a transaction block will provoke a warning
message. The state of the transaction is not affected. To nest transactions
within a transaction block, use savepoints.

So, if a transaction is started on a wrong/incorrect/unexpected mode it will
stay that way forever. So make sure a new connection is always first started
with a BEGIN or SET TRANSACTION statement.

--}

data Connection = Connection
        { _connectionStr :: String
        , _connectionServerId :: Integer
        , _connectionBizVersionId :: Integer
        , _connectionWriteAccess :: Bool
        , _connectionSemaphor :: MVar.MVar ()
        , _connectionInternal :: Internal.Connection
        }

toInternal :: Connection -> Internal.Connection
toInternal = _connectionInternal

connectRO :: String -> [Integer] -> IO Connection
connectRO connStr bizVersionNumber = connect False

connectRW :: String -> [Integer] -> IO Connection
connectRW connStr bizVersionNumber = connect True

connect :: Bool -> String -> [Integer] -> IO Connection
connect rw connStr bizVersionNumber = do
        schema <- Internal.connectPsql connStr
        serverId <- Internal.getServerId schema
        -- The business version must exists on the database.
        bizId <- Internal.getBizVersionId schema bizVersionNumber
        mVar <- MVar.newMVar ()
        return (Connection connStr serverId bizId rw mVar schema)

-- | Creates a new connection reusing the connection string and RW parameters.
_copy :: Connection -> IO Connection
_copy (Connection connStr serverId bizId rw _ _) = do
        schema <- Internal.connectPsql connStr
        mVar <- MVar.newMVar ()
        return (Connection connStr serverId bizId rw mVar schema)

close :: Connection -> IO ()
close (Connection _ _ _ _ mVar schema) = do
        -- | TODO: If someone closes the conection all future uses of the
        -- Connection object will block indefinitely.
        _ <- MVar.takeMVar mVar
        Internal.closePsql schema

--------------------------------------------------------------------------------

-- | Shard id must not change while the app is running.
getServerId :: Connection -> Integer
getServerId (Connection _ serverId _ _ _ _) = serverId

-- | Business version must not change while the app is running.
getBizVersionId :: Connection -> Integer
getBizVersionId (Connection _ _ bizVersionId _ _ _) = bizVersionId

--------------------------------------------------------------------------------

-- | The user can only directly start a transaction by calling this function
-- that ensures the connection and transaction are ReadOnly and Serializable.
-- The transaction is always rolledback or aborted on error. And the transaction
-- is retried indefinitely on serialization errors.
withTransactionRO :: Connection -> (Internal.Connection -> IO a) -> IO a
withTransactionRO conn action = withTransactionMVar
        conn
        -- Retry on serialization errors.
        (withTransactionRetry
                -- Begin action / aquire resources.
                Internal.beginTransactionRO
                -- Action to run after begin / aquiring resources.
                (\_ schema -> do
                        ans <- action schema
                        -- Always rollback an RO transaction.
                        return (Left ans)
                )
                -- Action to run before retrying.
                (\_ _ _ -> return ())
        )

-- | Just checks that the connection object is not read only and aquires an
-- exclusive lock on the object with ROLLBACK on error as a safety net.
-- Not exported. The user starts RW transactions with withBizMethod or
-- withBizMethodReuse.
withConnectionRW :: Connection
                 -> (Internal.Connection -> IO (Either SomeException ans))
                 -> IO ans
withConnectionRW (Connection _ _ _ False _ _) _ = do
        error "<2>Trying to start a RW transaction on an RO conn."
withConnectionRW conn action = do
        withTransactionMVar conn action

-- API to use the business method.
--------------------------------------------------------------------------------

{-- TODO: Distributed serializable transactions:

https://en.wikipedia.org/wiki/Global_serializability
https://en.wikipedia.org/wiki/Commitment_ordering

https://en.wikipedia.org/wiki/CAP_theorem
https://research.google.com/pubs/pub45855.html

https://wiki.postgresql.org/wiki/Serializable#Apparent_Serial_Order_of_Execution

https://www.postgresql.org/message-id/CACjxUsMKA6k-mDOdkos3k0i-KE4HFRwkd%3DPXPArYy4UabTd-LA%40mail.gmail.com
http://www.postgresql-archive.org/Sequences-txids-and-serial-order-of-transactions-td5907463.html
http://rhaas.blogspot.com.ar/2010/07/distributed-serialization-anomalies.html
http://hackage.haskell.org/package/DSTM
https://www.amazon.com/Distributed-Database-Management-Systems-Practical/dp/047040745X
http://www.ssrg.ece.vt.edu/papers/ppopp_2016_saad.pdf

--}

-- | Create's a new app transaction that will be logged on the database.
-- The user can ONLY start a ReadWrite transaction, that is always Serializable,
-- by calling this function.
-- The transaction is always rolledback or aborted on error. And the transaction
-- is retried indefinitely on serialization errors.
withBizMethod :: Connection -> String -> (Internal.Connection -> IO a) -> IO a
withBizMethod conn bizMethodName action = do
        -- Now the created business method call ID is not returned, but it may
        -- be needed later to tie the action to a user or an HTTP request.
        (_,_,ans) <- withBizMethod' conn bizMethodName action
        return ans

-- | TODO: Starts a new connection and runs the action on a parrallel thread.
-- When finished, closes the connection.
-- TODO: Better try implementing in software "nested" transactions.
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-ST.html
-- http://hackage.haskell.org/package/stm
withBizMethodAsync :: Connection
                   -> String
                   -> (Internal.Connection -> IO a)
                   -> IO (Async.Async a)
withBizMethodAsync conn bizMethodName action = do
        conn' <- _copy conn
        -- Now the created business method call ID is not returned, but it may
        -- be needed later to tie the action to a user or an HTTP request.
        async <- Async.async $ finally
                (do
                        (_,_,ans) <- withBizMethod' conn' bizMethodName action
                        return ans
                )
                (close conn')
        return async

-- | Make the transaction reusable if not already replicated.
-- Useful when a process updates regulary something and the intermediate values
-- are not important as this avoids replicating intermediate steps as much as
-- possible.
-- If not used with care all future replications may be inconsistent. Must be
-- used to update plain data values, the ones with no references at all.
withBizMethodReuse :: Connection
                   -> String
                   -> (Internal.Connection -> IO a)
                   -> IO a
withBizMethodReuse conn bizMethodName action = do
        -- First run the biz method as usual.
        eitherCommit <- withBizMethod' conn bizMethodName action
        -- Now clean for reuse.
        case eitherCommit of
                -- The action was rollbacked.
                (_,Nothing,ans) -> return ans
                -- The action was commited.
                (bizMethodId, (Just bizMethodCallId), ans) ->  do
                        let schema = toInternal conn
                        -- Run on an unimportant isolation level to delete the
                        -- not replicated business methods calls.
                        eitherAns <- try $ do
                                Internal.beginTransactionRWReadCommitted schema
                                -- Try to use index-only scans to avoid
                                -- serialization errors.
                                Internal.setSeqScanOff schema
                                -- Just in case use a limit of 1000.
                                -- To further avoid serialization errors, select
                                -- only the (N - 2) not replicated methods. So
                                -- if a simultaneous replicator is running it
                                -- should be processing the two methods we just
                                -- skipped.
                                ids <- Internal.getPreviousBizMethodsNotPublished
                                        schema
                                        bizMethodId
                                        bizMethodCallId
                                        (1000,2)
                                Internal.setSeqScanOn schema
                                Internal.deleteBizMethodsNotPublished schema ids
                                Internal.commit schema
                        -- Check if deleting the methods raised any exceptions.
                        case eitherAns of
                                -- Exceptions.
                                (Left e) -> do
                                        putStrLn "<4>Database.Business.withBizMethodReuse"
                                        putStrLn $ "<4>" ++ (show (e :: SomeException))
                                        Internal.rollback schema
                                        -- If the excetion is a serializable
                                        -- one, do nothing, do not try again,
                                        -- leave the method for replication.
                                        -- Else rethrow the exception.
                                        when
                                                (not $ Internal.shouldRetry $
                                                        fromException e
                                                )
                                                (throwIO e)
                                -- Ok!.
                                (Right _) -> return ()
                        return ans

-- Business method common code.
-- Returns the users's output plus bizMethodId and bizMethodCallId.
withBizMethod' :: Connection
               -> String
               -> (Internal.Connection -> IO a)
               -- With the action output return:
               -- - On rollback: only the bizMethodId.
               -- - On commit: the bizMethodId and bizMethodCallId.
               -> IO (Integer, Maybe Integer, a)
withBizMethod' conn bizMethodName action = do
        -- The global ID used to create the business method call is obtained
        -- first outside any transaction. This way on retry we can reuse it and
        -- log the errors with this ID even though the row will be created.
        globalId <- Internal.createGlobalId (toInternal conn)
        -- Call withConnectionRW that also checks that the connection is RW.
        withConnectionRW conn $ withTransactionRetry
                -- Begin action / aquire resources.
                (\schema -> do
                        Internal.beginTransactionRWSerial schema
                        -- The business method call must be created inside the
                        -- transaction because:
                        -- - On error: The business method call row must be 
                        -- - deleted.
                        -- - On retry: It must be recreated to preserve the
                        -- - time order. This is to keep an ordered log to later
                        -- - replicate without having inconsistencies.
                        bizIds <- Internal.createBizMethodCallWithId
                                schema
                                bizMethodName
                                version
                                globalId
                        return bizIds
                )
                -- Action to run after begin / aquiring resources.
                (\(bizMethodId, bizMethodCallId) schema -> finally
                        -- Only call Internal.setBizMethodIds inside
                        -- withConnectionRW so we are sure that no one else
                        -- set this IDs while we are executing the main action.
                        (do
                                Internal.setBizMethodIds
                                        schema
                                        (bizMethodId, bizMethodCallId)
                                -- The action of withConnectionRW.
                                eitherAns <- runBizMethod conn action
                                case eitherAns of
                                        -- If it receives Left: Will Rollback.
                                        (Left ans) -> return $ Left
                                                (bizMethodId, Nothing, ans)
                                        -- If it receives Right: Will Commit.
                                        (Right ans) -> do
                                                -- Notifies the end of the
                                                -- business method call.
                                                -- PostgreSQL won't notify until
                                                -- the transaction commits.
                                                Internal.notifyBizMethodCall
                                                        schema
                                                return $ Right $
                                                        (
                                                                  bizMethodId
                                                                , (Just
                                                                        bizMethodCallId
                                                                  )
                                                                , ans
                                                        )
                        )
                        -- Always always always reset the schema, even on error
                        -- and even if we are not sure that this was set
                        -- correctly.
                        (Internal.resetBizMethodIds schema)
                )
                -- Action to run before retrying.
                (\(_, bizMethodCallId) e schema -> do
                        -- TODO: This needs to be run inside a try ??
                        -- Run on an unimportant isolation level to store it.
                        Internal.beginTransactionRWReadCommitted schema
                        _ <- Internal.createBizMethodCallRetry
                                schema
                                (show e)
                                bizMethodCallId
                        Internal.commit schema
                        return ()
                )

-- Database queries were used to check if a finished business method had at
-- least one schema action or else it could be deleted. But this extra check
-- could augment serialization errors due to the concurrent SELECT, DELETE on
-- table public.schema_actions.
-- Now we do it here with a counter on Internal.Connection.
runBizMethod :: Connection
             -> (Internal.Connection -> IO a)
             -> IO (Either a a)
runBizMethod conn action = do
        let schema = toInternal conn
        -- Prepare schema state.
        Internal.resetBizMethodDataMethodsCounter schema
        -- Run action.
        ans <- action schema
        -- Get schema state.
        -- As this function is still run inside withTransactionMVar we can be
        -- sure that the state is untoched as there are no other functions
        -- inside this module that use the schema state.
        dataMethodsCounter <- Internal.getBizMethodDataMethodsCounter schema
        -- Decide return value.
        if (dataMethodsCounter <= 0)
                -- If no data methods Rollback.
                then return (Left ans)
                -- If at least one data method, can commit.
                else return (Right ans)

--------------------------------------------------------------------------------

withTransactionMVar :: Connection
                    -- Return Left for 'expected' exceptions during the normal
                    -- execution of SQL commands. This ones are assumed to be
                    -- correctly treated and are just rethrown.
                    -- Any other exception caught was unexpected, like during
                    -- BEGIN or a timeout or any other asynchronous exceptions,
                    -- and we ROLLBACK just in case.
                    -> (Internal.Connection -> IO (Either SomeException ans))
                    -> IO ans
withTransactionMVar (Connection _ _ _ _ mVar schema) action = do
        mask $ \restore -> do
                -- -- MaskedInterruptible --.
                -- The MVar is taken here so while retrying the resource is
                -- still aquired and no other method can interrupt it. This
                -- ensures that within a single connection the order is
                -- preserved even when retrying.
                _ <- MVar.takeMVar mVar
                -- Start the "BEGIN; QUERY;COMMIT;" loop.
                -- The order here is very important as doing try after restore
                -- won't catch every exception. 
                eitherError <- try $ restore $ do
                        -- -- Unmasked --.
                        -- Everything is run with interruptions unmasked so all
                        -- of them can be stoppped with a timeout (important as
                        -- every call uses a network connection).
                        action schema
                -- -- MaskedInterruptible --.
                -- Its a feature not a bug that if begin transaction fails we
                -- still call rollback. We always try to rollback on error!!!!
                -- PSQL Docs: "Issuing ROLLBACK outside of a transaction block
                -- emits a warning and otherwise has no effect."
                either
                        -- Exception returned.
                        (\_ -> handle
                                (\e -> do
                                        -- An exception on rollback is ignored,
                                        -- just show it.
                                        putStrLn "<4>Database.Business.withTransactionMVar: rollback."
                                        putStrLn $ "<4>" ++ (show (e :: SomeException))
                                )
                                -- As postgres library uses sockets/network to
                                -- communicate this is an Interruptible
                                -- operation. I think restore is not needed.
                                (Internal.rollback schema)
                        )
                        -- No Exception returned.
                        (\_ -> return ())
                        eitherError
                -- Return the MVar as soon as possible so a new transaction can
                -- occur inmediately.
                -- Warning: anybody can start modifying the schema state.
                MVar.putMVar mVar ()
                -- Now we can check for exceptions to throw.
                -- Asynchronous exceptions are not needed masked any more, all
                -- the critical parts are done, so let the asyncs begin!!!
                restore $ case eitherError of
                        -- The exception uncaught by withTransactionRetry.
                        (Left e) -> do
                                -- Show and throw the error.
                                putStrLn "<3>Database.Business.withTransactionMVar: exception uncaught by withTransactionRetry."
                                putStrLn $ "<3>" ++ (show (e :: SomeException))
                                throwIO e
                        -- Ans or an exception caught by withTransactionRetry.
                        (Right eitherAns) -> case eitherAns of
                                -- Error that are not serialization related!
                                (Left e) -> throwIO e
                                -- Everything OK!
                                (Right ans) -> return ans

-- If the action returns Left, rollback, else it can be commited.
-- If a serialization error occurs the transaction is repeated indefinitely.
-- Error: "libpq: failed (another command is already in progress)"
-- https://github.com/lpsmith/postgresql-simple/issues/177
withTransactionRetry :: (Internal.Connection -> IO b)
                     -> (b -> Internal.Connection -> IO (Either ans ans))
                     -> (b -> SomeException -> Internal.Connection -> IO ())
                     -> Internal.Connection
                     -> IO (Either SomeException ans)
withTransactionRetry begin action retry schema = do
        -- If the BEGIN TRANSACTION fails, fail everything, no catch here.
        beginAns <- begin schema
        -- Now catch to look for serialization errors to retry.
        eitherAns <- try $ do
                -- Run the desired actions.
                eitherCommit <- action beginAns schema
                -- ROLLBACK or COMMIT;
                case eitherCommit of
                        -- The action requested a rollback.
                        (Left ans) -> do
                                Internal.rollback schema
                                return ans
                        -- The action says we can commit.
                        (Right ans) -> do
                                Internal.commit schema
                                return ans
        -- Check for serialization errors to retry.
        case eitherAns of
                -- An exception occurred.
                (Left e) -> do
                        Internal.rollback schema
                        -- If a retry makes sense start over again.
                        if (Internal.shouldRetry $ fromException e)
                                -- Retry.
                                then do
                                        -- Extra user supplied action on retry.
                                        retry beginAns e schema
                                        -- WARNING: Retry using
                                        -- withTransactionRetry NOT
                                        -- withTransactionMVar, else deadlock.
                                        withTransactionRetry
                                                begin
                                                action
                                                retry
                                                schema
                                -- Other type of error, bye!
                                else return (Left e)
                -- No exception caught.
                (Right ans) -> return (Right ans)

--------------------------------------------------------------------------------

-- | A sequential scan will always necessitate a relation-level predicate lock.
-- This can result in an increased rate of serialization failures. It may be
-- helpful to encourage the use of index scans by reducing random_page_cost
-- and/or increasing cpu_tuple_cost. Be sure to weigh any decrease in
-- transaction rollbacks and restarts against any overall change in query
-- execution time.
-- This should be used by the Business methods in accordance with the database
-- administrators that overlook every data method.
withEncourageIndexScan :: Internal.Connection
                       -> (Internal.Connection -> IO a)
                       -> IO a
withEncourageIndexScan schema action = do
        -- Schema module bounds this change to the current transaction.
        -- It is assumed that the business layer can only aquire an
        -- Internal.Connection object by starting a transaction.
        Internal.setSeqScanOff schema
        ans <- action schema
        Internal.setSeqScanOn schema
        return ans

--------------------------------------------------------------------------------

-- | Fetch business methods calls from the publisher's database (may be
-- different than the original caller of the business method) that the provided
-- destination shard ID is subscribed and publish them on the destination's
-- database. Notice that the intended final shard destination may be different
-- than the provided destination connection.
-- A two-phase process.
-- The first one opens both transactions simultaneusly to fecth from publisher
-- and publish to destination method calls that are not on the publishers log or
-- have two-way-commit False.
-- The second phase reopens both transactions simultaneusly to mark
-- two-way-commit as True.
publishBizMethods :: Connection
                  -> Connection
                  -> Integer
                  -> Int
                  -> IO [(Integer,Integer)]
publishBizMethods connPub connDst dstServerId limit = do
        -- Phase 1.
        -- Fetch and publish up to limit business method calls.
        publicationIds <- publishBizMethodsPhase1
                connPub connDst dstServerId limit
        -- Phase 2.
        -- Set the two-way-commit flag as OK (True) on both databases.
        -- If something fails here, none will have two-way-commit as OK.
        case publicationIds of
                -- Do not open any transaction, just exit.
                -- The answer of publishBizMethodsPhase1 also includes the
                -- publications without two-way-commit.
                [] -> return []
                -- Process.
                _ -> do
                        _ <- publishBizMethodsPhase2
                                connPub connDst publicationIds
                        return publicationIds

-- Fetch business methods from the publisher that does not have them on its
-- publish log for the provided destination shard id or have two-way-commit
-- false. And publish them by adding them to the publish log on both servers but
-- with two-way-commit false and storing the business method call on the
-- destination.
-- The final shard destination may be different to the provided connection.
publishBizMethodsPhase1 :: Connection
                        -> Connection
                        -> Integer
                        -> Int
                        -> IO [ (Integer, Integer) ]
publishBizMethodsPhase1 connPub connDst dstServerId limit = withConnectionRW
        connPub
        -- Exclusive lock on publisher's connection object with rollback as a
        -- safety net.
        (\schmPub -> do
                -- Serializable transaction on publisher. No retying!
                Internal.beginTransactionRWSerial schmPub
                -- Fetch from publisher methods not published to destination.
                -- Also fetches when two-way-commit is not true.
                notPublished <- Subs.getBizMethodsNotPublishedFirst
                        schmPub
                        dstServerId
                        limit
                -- Was something to publish found?
                publicationIds <- case notPublished of
                        -- If nothing to publish, exit early with no transaction
                        -- opened on the destination.
                        [] -> do
                                Internal.rollback schmPub
                                return []
                        -- Else open transaction on destination.
                        subsBizMethodsCalls -> do
                                publicationIds <- withConnectionRW
                                        connDst
                                        -- Exclusive lock on destination's
                                        -- connection object with rollback as a
                                        -- safety net.
                                        (\schmDst -> do
                                                -- Serializable transaction on
                                                -- destination. No retying!
                                                Internal.beginTransactionRWSerial
                                                        schmDst
                                                -- Do the publishing.
                                                ans <- mapM
                                                        (publish
                                                                schmPub
                                                                schmDst
                                                                (getServerId
                                                                        connPub
                                                                )
                                                                dstServerId
                                                        )
                                                        subsBizMethodsCalls
                                                -- First commit the destination
                                                -- which is a bigger process.
                                                Internal.commit schmDst
                                                return (Right ans)
                                        )
                                -- If something fails here (before publisher
                                -- commits) the two-way-commit resolves it on a
                                -- later call to this function.
                                -- The problem will be that the business method
                                -- call is published on the destination (but
                                -- with two-way-commit being false) but this is
                                -- completely unknown to the publisher because
                                -- it has nothing on its database related to
                                -- this publication.
                                Internal.commit schmPub
                                return publicationIds
                -- Right: No exceptions caught.
                return (Right publicationIds)
        )

-- Add the business method to both publish logs and the business method call
-- data to the destination.
publish :: Internal.Connection
        -> Internal.Connection
        -> Integer
        -> Integer
        -> (Integer, Integer, Subs.BizMethodCall)
        -> IO (Integer, Integer)
publish schmPub schmDst serverIdPub serverIdDst (subId, _, bizMethodCall) = do
        -- Get the publishing status on both servers.
        -- The publisher shard ID is not given to this two functions. So if
        -- another publisher has already processed somehow this method call its
        -- status will be returned.
        pubPub <- Subs.getPublicationTwoWayCommit schmPub
                bizMethodCall serverIdDst
        pubDst <- Subs.getPublicationTwoWayCommit schmDst
                 bizMethodCall serverIdDst
        -- Check for partial publishings.
        case (pubPub, pubDst) of
                -- Nothing ever happened with this method or both rollbacked.
                -- May happen due to serialization errors, we don't retry any
                -- part of the publishing process.
                (Nothing, Nothing) -> do
                        -- Insert biz method call with its data methods.
                        -- Unless self-publishing or cyclic-publishing the
                        -- method call should not already exist.
                        _ <- Subs.insertBizMethodCall schmDst bizMethodCall
                        -- Now insert the data method calls.
                        dataMethodCalls <- Subs.getBizMethodCallDataMethodCalls
                                schmPub
                                bizMethodCall
                        _ <- mapM
                                (Subs.insertDataMethodCall schmDst)
                                dataMethodCalls
                        -- Publications log on publisher.
                        pubIdPub <- Subs.insertPublication
                                schmPub
                                subId
                                bizMethodCall
                                serverIdPub
                                serverIdDst
                        -- Publications log on destination.
                        pubIdDst <- Subs.insertPublication
                                schmDst
                                subId
                                bizMethodCall
                                serverIdPub
                                serverIdDst
                        return (pubIdPub, pubIdDst)
                -- When destination commits phase 1 and an error occurs just
                -- before publisher commits.
                (Nothing, (Just (pubIdDst, pubServerIdDst, False))) -> do
                        -- Check that the destination does not have this method
                        -- call on its publish log but from another publisher.
                        if serverIdPub /= pubServerIdDst
                                then do
                                        -- Not supported right now!
                                        error $ "<1>Cyclic publication: " ++ (show (pubPub, pubDst))
                                else do
                                        pubIdPub <- Subs.insertPublication
                                                schmPub
                                                subId
                                                bizMethodCall
                                                serverIdPub
                                                serverIdDst
                                        return (pubIdPub, pubIdDst)
                -- The destination got this biz method call from another
                -- publisher. If not how can two-way-commit be true on the
                -- destination and Nothing on the publisher? That would mean
                -- that Phase-1 and Phase-2 are completed on the destination and
                -- none on the publisher.
                (Nothing, (Just (_, _, True))) -> do
                        -- Not supported right now!
                        error $ "<1>Cyclic publication: " ++ (show (pubPub, pubDst))
                -- Both replication are done but at least one has not marked
                -- two way commit. We don't differentiate which one, just go
                -- to phase 2.
                ((Just (pubIdPub, pubServerIdPub, False)), (Just (pubIdDst, pubServerIdDst, False))) -> do
                        -- Check that both publication logs entries are from the
                        -- same publishers.
                        if pubServerIdPub /= pubServerIdDst
                                then do
                                        -- Not supported right now!
                                        error $ "<1>Cyclic publication: " ++ (show (pubPub, pubDst))
                                else do
                                        return (pubIdPub, pubIdDst)
                -- This is an impossible!
                -- ((Just (pubIdPub, _)), Nothing) -> do
                wtf -> do
                        error $ "<1>The impossible just happened: " ++ (show wtf)

publishBizMethodsPhase2 :: Connection
                        -> Connection
                        -> [ (Integer, Integer) ]
                        -> IO [(Integer, Integer)]
publishBizMethodsPhase2 connPub connDst publicationIds = withConnectionRW
        connPub
        (\schmPub -> do
                -- Serializable transaction on publisher. No retying!
                Internal.beginTransactionRWSerial schmPub
                rowsUpdated <- withConnectionRW
                        connDst
                        (\schmDst -> do
                                -- Serializable transaction on destination.
                                -- No retying!
                                Internal.beginTransactionRWSerial schmDst
                                rowsUpdated <- mapM
                                        (\(pubIdPub, pubIdDst) -> do
                                                rPub <- Subs.setPublicationTwoWayCommit
                                                        schmPub
                                                        pubIdPub
                                                rDst <- Subs.setPublicationTwoWayCommit
                                                        schmDst
                                                        pubIdDst
                                                return (rPub, rDst)
                                        )
                                        publicationIds
                                Internal.commit schmDst
                                -- Right: No exceptions caught.
                                return (Right rowsUpdated)
                        )
                -- If something fails here (before publisher commits) the
                -- two-way-commit resolves it. The problem is that the business
                -- method is replicated on both origin and destination but only
                -- the destination has two way commits.
                Internal.commit schmPub
                -- Right: No exceptions caught.
                return (Right rowsUpdated)
        )

--------------------------------------------------------------------------------

-- | Run business methods calls published but not replicated on the database.
-- The publication only contains the metadata of the method call, this function
-- runs the method.
replicateBizMethods :: Connection -> Int -> IO [Integer]
replicateBizMethods conn limit = withConnectionRW conn
        (\schm -> do
                -- Serializable transaction on publisher. No retying!
                Internal.beginTransactionRWSerial schm
                bizCalls <- Subs.getBizMethodsNotReplicatedFirst schm limit
                replicationsIds <- mapM
                        (\bmc -> finally
                                (do
                                        let bizMethodId = Subs.bizMethodCallBusinessMethodId
                                                bmc
                                        let bizMethodCallId = Subs.bizMethodCallId
                                                bmc
                                        Internal.setBizMethodIds
                                                schm
                                                (bizMethodId, bizMethodCallId)
                                        Subs.replicateBizMethodCall schm bmc
                                        ans <- Subs.insertPublicationReplication
                                                schm bmc
                                        Internal.notifyBizMethodCall
                                                schm
                                        return ans
                                )
                                (Internal.resetBizMethodIds schm)
                        )
                        bizCalls
                Internal.commit schm
                -- Right: No exceptions caught.
                return (Right replicationsIds)
        )
