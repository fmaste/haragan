{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Database.Schema.Subscriptions (

          BizMethodCall (bizMethodCallId, bizMethodCallBusinessMethodId)
        , getBizMethodsNotPublishedFirst
        , getBizMethodsNotReplicatedFirst

        , getPublicationTwoWayCommit
        , setPublicationTwoWayCommit

        , insertBizMethodCall
        , DataMethodCall ()
        , getBizMethodCallDataMethodCalls
        , insertDataMethodCall
        , replicateBizMethodCall

        , insertPublication
        , insertPublicationReplication

) where

--------------------------------------------------------------------------------

-- base.
--
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
-- Package: text.
import qualified Data.Text as Text
-- Package: time.
import qualified Data.Time as Time
-- Home Package: haragan.
import qualified Database.Internal as Internal

--------------------------------------------------------------------------------

data BizMethodCall = BizMethodCall {
          bizMethodCallId :: Integer
        , bizMethodCallCreationTime :: Time.UTCTime
        , bizMethodCallDisplayName :: Maybe Text.Text
        , bizMethodCallBusinessMethodId :: Integer
        , bizMethodCallBusinessVersionId :: Integer
        , bizMethodCallSchemaVersionId :: Integer
        , bizMethodCallOriginShardId :: Integer
} deriving (Eq, Show)

-- | Of a given destination shard ID get its subscribed business method calls
-- that this database connection has that are also not already published (as
-- known by this database connection public.subscriptions_publications table)
-- on the provided destination shard ID.
-- The subscriptions details, the information of which business methods and from
-- which origins shards, is stored on the database public.subscriptions_? table
-- of the provided connection.
-- Brings all the business method calls from oldest to newest, there is no time
-- filter available (all or nothing). This is the only way we can be sure that
-- every sync process is done on ascending order, so subscriptions can be
-- resumed from another shard without extra third parties syncing.
-- TODO: To allow resuming, the destination must provide its subscriptions last
-- method call creation timestamps.
-- The method calls originated on the destination shard are filtered out.
getBizMethodsNotPublishedFirst :: Internal.Connection
                               -> Integer
                               -> Int
                               -- With subscription ID and destination shard ID.
                               -> IO [(Integer, Integer, BizMethodCall)]
getBizMethodsNotPublishedFirst conn dstShardId limit = do
        ans <- PSQL.query (Internal.toPsql conn)
                -- Use a subquery to get all the unique (subs,method,origin,dst)
                -- tuples this destination shard is subscribed to.
                -- As long as:
                -- -- subscriptions_shards has this UNIQUE:
                -- -- UNIQUE(subscriptions_id, origin_shard_id, destination_shard_id)
                -- -- and subscriptions_business_methods has this UNIQUE:
                -- -- UNIQUE(business_methods_id)
                -- We know business_methods_id wont be repeated.
                (" WITH subscriptions AS ( "
                        <> " SELECT DISTINCT "
                                <> "   sbm.subscriptions_id AS id "
                                <> " , sbm.business_methods_id AS business_methods_id "
                                <> " , ss.origin_shard_id AS origin_shard_id "
                                <> " , ss.destination_shard_id AS destination_shard_id "
                        <> " FROM "
                                <> "   public.subscriptions_business_methods AS sbm "
                                <> " , public.subscriptions_shards AS ss "
                        <> " WHERE "
                                <> " sbm.subscriptions_id = ss.subscriptions_id "
                                <> " AND "
                                -- Avoid self publishing here no matter what.
                                <> " NOT ss.origin_shard_id = ? "
                                <> " AND "
                                <> " ss.destination_shard_id = ? "
                        <> " ORDER BY "
                                <> "   sbm.subscriptions_id "
                                <> " , sbm.business_methods_id "
                                <> " , ss.origin_shard_id "
                                <> " , ss.destination_shard_id "
                <> " ) "
                -- Get the subscribed methods calls not already published.
                -- Can't obtain repeated rows because the subscriptions are for
                -- an specific origin-destination shard ID, and a biz method can
                -- only belong to one subscription.
                <> " SELECT "
                        <> "   subscriptions.id "
                        <> " , subscriptions.destination_shard_id "
                        <> " , bmc.id, bmc.creation_time, bmc.display_name "
                        <> " , bmc.business_methods_id "
                        <> " , bmc.business_versions_id "
                        <> " , bmc.schema_versions_id "
                        <> " , bmc.origin_shard_id "
                        <> " FROM "
                                -- Join biz method calls with subscriptions.
                                <> " public.business_methods_calls AS bmc "
                                <> " INNER JOIN "
                                <> " subscriptions "
                                <> " ON "
                                        -- The biz method ID has a subscription for this destination shard.
                                        <> " bmc.business_methods_id = subscriptions.business_methods_id "
                                        -- Must match the whole (method,origin) tuple.
                                        <> " AND "
                                        -- The biz method call origin shard has a subscription for this destination shard.
                                        <> " bmc.origin_shard_id = subscriptions.origin_shard_id "
                        <> " WHERE "
                                -- Avoid self publishing here no matter what.
                                <> " NOT bmc.origin_shard_id = ? "
                                <> " AND "
                                -- The biz method call is not already published
                                -- to this destination shard or was not obtained
                                -- from this destination shard (the publisher).
                                <> " bmc.id NOT IN ( "
                                        <> " SELECT business_methods_calls_id "
                                        <> " FROM public.subscriptions_publications "
                                        <> " WHERE "
                                                <> " ( "
                                                <> " destination_shard_id = ? "
                                                <> " AND "
                                                -- If not two_way_commit,
                                                -- consider it as not published
                                                -- so it is treated acordingly.
                                                <> " two_way_commit "
                                                <> " ) "
                                                <> " OR "
                                                <> " ( "
                                                -- Do not push to this
                                                -- destination shard what was
                                                -- already obtained from that
                                                -- same shard.
                                                <> " publisher_shard_id = ? "
                                                -- two_way_commit does not 
                                                -- matter, no republishing.
                                                <> " ) "
                                <> " ) "
                        -- The creation_time can repeat, use ID to resolve.
                        <> " ORDER BY bmc.creation_time ASC, bmc.id ASC "
                        <> " LIMIT ? OFFSET 0 "
                        <> " ; "
                )
                (
                          dstShardId
                        , dstShardId
                        , dstShardId
                        , dstShardId
                        , dstShardId
                        , limit
                )
        return $ map toSubBizMethod ans

toSubBizMethod :: (
                          Integer
                        , Integer
                        , Integer
                        , Time.UTCTime
                        , Maybe Text.Text
                        , Integer
                        , Integer
                        , Integer
                        , Integer
                )
            -> (Integer, Integer, BizMethodCall)
toSubBizMethod (subId, dstId, bmcId, time, bmcName, bmId, bvId, svId, shardId) =
        (
                        subId, dstId
                ,
                        BizMethodCall
                        {
                                  bizMethodCallId = bmcId
                                , bizMethodCallCreationTime = time
                                , bizMethodCallDisplayName = bmcName
                                , bizMethodCallBusinessMethodId = bmId
                                , bizMethodCallBusinessVersionId = bvId
                                , bizMethodCallSchemaVersionId = svId
                                , bizMethodCallOriginShardId = shardId
                        }
        )

getBizMethodsNotReplicatedFirst :: Internal.Connection
                                -> Int
                                -> IO [BizMethodCall]
getBizMethodsNotReplicatedFirst conn limit = do
        ans <- PSQL.query (Internal.toPsql conn)
                -- Using DISTINCT because publication of the same business
                -- method call can appear on the log with different destinations.
                (" SELECT DISTINCT ON ( bmc.id, bmc.creation_time ) "
                        <> "   bmc.id, bmc.creation_time, bmc.display_name "
                        <> " , bmc.business_methods_id "
                        <> " , bmc.business_versions_id "
                        <> " , bmc.schema_versions_id "
                        <> " , bmc.origin_shard_id "
                        <> " FROM "
                                <> " public.business_methods_calls AS bmc "
                                <> " INNER JOIN "
                                <> " public.subscriptions_publications AS sp "
                                <> " ON "
                                        <> " sp.business_methods_calls_id = bmc.id "
                                        <> " AND "
                                        <> " sp.destination_shard_id = public.shard_id() "
                        <> " WHERE "
                                -- Only replicate if on the same schema version.
                                <> " bmc.schema_versions_id = public.schema_version_id() "
                                <> " AND "
                                -- Do not replicate partial publications.
                                -- Replication could be running and in the
                                -- middle of the two-transactions publishing
                                -- process.
                                <> " sp.two_way_commit "
                                <> " AND "
                                -- Not already replicated.
                                <> " NOT EXISTS ( "
                                        <> " SELECT id "
                                        <> " FROM public.subscriptions_publications_replications "
                                        <> " WHERE "
                                                <> " business_methods_calls_id = sp.business_methods_calls_id"
                                                <> " AND "
                                                <> " destination_shard_id = sp.destination_shard_id "
                                <> " ) "
                        -- The creation_time can repeat, use ID to resolve.
                        <> " ORDER BY bmc.creation_time ASC, bmc.id ASC "
                        <> " LIMIT ? OFFSET 0 "
                        <> " ; "
                )
                (PSQL.Only limit)
        return $ map toBizMethod ans

toBizMethod :: (
                          Integer
                        , Time.UTCTime
                        , Maybe Text.Text
                        , Integer
                        , Integer
                        , Integer
                        , Integer
               )
            -> BizMethodCall
toBizMethod (bmcId, time, bmcName, bmId, bvId, svId, shardId) =
        BizMethodCall
                {
                          bizMethodCallId = bmcId
                        , bizMethodCallCreationTime = time
                        , bizMethodCallDisplayName = bmcName
                        , bizMethodCallBusinessMethodId = bmId
                        , bizMethodCallBusinessVersionId = bvId
                        , bizMethodCallSchemaVersionId = svId
                        , bizMethodCallOriginShardId = shardId
                }

--------------------------------------------------------------------------------

-- | The publication of a business method call to an specific destination shard.
-- A shard can publish the same method call to multiple destinations.
getPublicationTwoWayCommit :: Internal.Connection
                           -> BizMethodCall
                           -> Integer
                           -> IO (Maybe (Integer, Integer, Bool))
getPublicationTwoWayCommit conn bizMethodCall dstShardId = do
        ans <- PSQL.query (Internal.toPsql conn)
                (" SELECT id, publisher_shard_id, two_way_commit "
                        <> " FROM public.subscriptions_publications "
                        <> " WHERE "
                                <> " business_methods_calls_id = ? "
                                <> " AND "
                                <> " destination_shard_id = ? "
                        <> " ; "
                )
                (bizMethodCallId bizMethodCall, dstShardId)
        case ans of
                ((spId, pubId, bool):[]) -> return $ Just (spId, pubId, bool)
                [] -> return Nothing
                wtf -> error (show wtf)

-- TODO: Return True or False if updated or not.
setPublicationTwoWayCommit :: Internal.Connection -> Integer -> IO Integer
setPublicationTwoWayCommit conn publicationId = do
        rowUpdated <- PSQL.execute (Internal.toPsql conn)
                (" UPDATE public.subscriptions_publications "
                        <> " SET two_way_commit = TRUE "
                        <> " WHERE "
                                <> " id = ? "
                                <> " AND "
                                <> " NOT two_way_commit "
                        <> " ; "
                )
                (PSQL.Only publicationId)
        return (toInteger rowUpdated)

--------------------------------------------------------------------------------

insertBizMethodCall :: Internal.Connection -> BizMethodCall -> IO Integer
insertBizMethodCall conn bizMethodCall = do
        _ <- PSQL.execute (Internal.toPsql conn)
                (" INSERT INTO public.business_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , business_methods_id "
                                <> " , business_versions_id "
                                <> " , schema_versions_id "
                                <> " , origin_shard_id "
                        <> " ) "
                        <> " VALUES (?,?,?,?,?,?,?) "
                        <> " ; "
                )
                (
                          bizMethodCallId bizMethodCall
                        , bizMethodCallCreationTime bizMethodCall
                        , bizMethodCallDisplayName bizMethodCall
                        , bizMethodCallBusinessMethodId bizMethodCall
                        , bizMethodCallBusinessVersionId bizMethodCall
                        , bizMethodCallSchemaVersionId bizMethodCall
                        , bizMethodCallOriginShardId bizMethodCall
                )
        -- Return the biz method call ID.
        return (bizMethodCallId bizMethodCall)

data DataMethodCall = DataMethodCall {
          dataMethodCallId :: Integer
        , dataMethodCallCreationTime :: Time.UTCTime
        , dataMethodCallDisplayName :: Maybe Text.Text
        , dataMethodCallBusinessMethodCallId :: Integer
        , dataMethodCallDataMethodId :: Integer
        , dataMethodCallDataMethodSchema :: Text.Text
        , dataMethodCallDataMethodTable :: Text.Text
        , dataMethodCallDataMethodVerb :: Text.Text
        , dataMethodCallRowId :: Maybe Integer
        , dataMethodCallRowData :: Maybe Aeson.Value
} deriving (Eq, Show)

getBizMethodCallDataMethodCalls :: Internal.Connection
                                -> BizMethodCall
                                -> IO [DataMethodCall]
getBizMethodCallDataMethodCalls conn bizMethod = do
        ans <- PSQL.query (Internal.toPsql conn)
                (" SELECT "
                        <> "   dmc.id, dmc.creation_time, dmc.display_name "
                        <> " , dmc.business_methods_calls_id "
                        <> " , dmc.data_methods_id "
                        <> " , dm.schema_name, dm.table_name "
                        <> " , dm.verb "
                        <> " , dmc.row_id, dmc.row_data "
                        <> " FROM "
                                <> "   public.data_methods AS dm "
                                <> " , public.data_methods_calls AS dmc "
                        <> " WHERE "
                                <> " dmc.data_methods_id = dm.id "
                                <> " AND "
                                <> " dmc.business_methods_calls_id = ? "
                        -- The creation_time can repeat, use ID to resolve.
                        <> " ORDER BY dmc.creation_time, dmc.id ASC "
                        <> " ; "
                )
                (PSQL.Only $ bizMethodCallId bizMethod)
        return $ map
                (\(dmcId, time, dmcName, bId, dmId, s, t, v, rId, rD) ->
                        DataMethodCall {
                                  dataMethodCallId = dmcId
                                , dataMethodCallCreationTime = time
                                , dataMethodCallDisplayName = dmcName
                                , dataMethodCallBusinessMethodCallId = bId
                                , dataMethodCallDataMethodId = dmId
                                , dataMethodCallDataMethodSchema = s
                                , dataMethodCallDataMethodTable = t
                                , dataMethodCallDataMethodVerb = v
                                , dataMethodCallRowId = rId
                                , dataMethodCallRowData = rD
                        }
                )
                ans

insertDataMethodCall :: Internal.Connection -> DataMethodCall -> IO ()
insertDataMethodCall conn dataMethod = do
        _ <- PSQL.execute (Internal.toPsql conn)
                (" INSERT INTO public.data_methods_calls "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , business_methods_calls_id "
                                <> " , data_methods_id "
                                <> " , row_id "
                                <> " , row_data "
                        <> " ) "
                        <> " VALUES (?,?,?,?,?,?,?) "
                        <> " ; "
                )
                (
                          dataMethodCallId dataMethod
                        , dataMethodCallCreationTime dataMethod
                        , dataMethodCallDisplayName dataMethod
                        , dataMethodCallBusinessMethodCallId dataMethod
                        , dataMethodCallDataMethodId dataMethod
                        , dataMethodCallRowId dataMethod
                        , dataMethodCallRowData dataMethod
                )
        return ()

replicateBizMethodCall :: Internal.Connection -> BizMethodCall -> IO ()
replicateBizMethodCall conn bizMethod = do
        dataMethods <- getBizMethodCallDataMethodCalls conn bizMethod
        mapM_
                (\dmc -> Internal.replicateDataMethodCall
                        conn
                        (dataMethodCallId dmc)
                        (Text.unpack $ dataMethodCallDataMethodVerb dmc)
                        (
                                  dataMethodCallDataMethodSchema dmc
                                , dataMethodCallDataMethodTable dmc
                        )
                        (dataMethodCallRowId dmc)
                        (dataMethodCallRowData dmc)
                )
                dataMethods
        return ()

--------------------------------------------------------------------------------

insertPublication :: Internal.Connection
                  -> Integer
                  -> BizMethodCall
                  -> Integer
                  -> Integer
                  -> IO Integer
insertPublication conn subscriptionId bizMethod pubShardId dstShardId = do
        [(PSQL.Only ans)] <- PSQL.query (Internal.toPsql conn)
                (" INSERT INTO public.subscriptions_publications "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , business_methods_id "
                                <> " , business_methods_calls_id "
                                <> " , subscriptions_id "
                                <> " , publisher_shard_id "
                                <> " , origin_shard_id "
                                <> " , destination_shard_id "
                                <> " , two_way_commit "
                        <> " ) "
                        <> " VALUES (DEFAULT,DEFAULT,DEFAULT,?,?,?,?,?,?,FALSE) "
                        <> " RETURNING ID "
                        <> " ; "
                )
                (
                          bizMethodCallBusinessMethodId bizMethod
                        , bizMethodCallId bizMethod
                        , subscriptionId
                        , pubShardId
                        , bizMethodCallOriginShardId bizMethod
                        , dstShardId
                )
        return ans

insertPublicationReplication :: Internal.Connection -> BizMethodCall -> IO Integer
insertPublicationReplication conn bizMethod = do
        [(PSQL.Only ans)] <- PSQL.query (Internal.toPsql conn)
                (" INSERT INTO public.subscriptions_publications_replications "
                        <> " ( "
                                <> "   id "
                                <> " , creation_time "
                                <> " , display_name "
                                <> " , business_methods_calls_id "
                                <> " , destination_shard_id "
                        <> " ) "
                        <> " VALUES (DEFAULT,DEFAULT,DEFAULT,?,public.shard_id()) "
                        <> " RETURNING ID "
                        <> " ; "
                )
                (PSQL.Only $ bizMethodCallId bizMethod)
        return ans

--------------------------------------------------------------------------------
