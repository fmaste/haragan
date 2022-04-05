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
publishBizMethods connPub connDst dstShardId limit = do
        -- Phase 1.
        -- Fetch and publish up to limit business method calls.
        publicationIds <- publishBizMethodsPhase1
                connPub connDst dstShardId limit
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
publishBizMethodsPhase1 connPub connDst dstShardId limit = withConnectionRW
        connPub
        -- Exclusive lock on publisher's connection object with rollback as a
        -- safety net.
        (\schmPub -> do
                -- Serializable transaction on publisher. No retying!
                Schema.beginTransactionRWSerial schmPub
                -- Fetch from publisher methods not published to destination.
                -- Also fetches when two-way-commit is not true.
                notPublished <- Subs.getBizMethodsNotPublishedFirst
                        schmPub
                        dstShardId
                        limit
                -- Was something to publish found?
                publicationIds <- case notPublished of
                        -- If nothing to publish, exit early with no transaction
                        -- opened on the destination.
                        [] -> do
                                Schema.rollback schmPub
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
                                                Schema.beginTransactionRWSerial
                                                        schmDst
                                                -- Do the publishing.
                                                ans <- mapM
                                                        (publish
                                                                schmPub
                                                                schmDst
                                                                (getShardId
                                                                        connPub
                                                                )
                                                                dstShardId
                                                        )
                                                        subsBizMethodsCalls
                                                -- First commit the destination
                                                -- which is a bigger process.
                                                Schema.commit schmDst
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
                                Schema.commit schmPub
                                return publicationIds
                -- Right: No exceptions caught.
                return (Right publicationIds)
        )

-- Add the business method to both publish logs and the business method call
-- data to the destination.
publish :: Schema.Connection
        -> Schema.Connection
        -> Integer
        -> Integer
        -> (Integer, Integer, Subs.BizMethodCall)
        -> IO (Integer, Integer)
publish schmPub schmDst shardIdPub shardIdDst (subId, _, bizMethodCall) = do
        -- Get the publishing status on both servers.
        -- The publisher shard ID is not given to this two functions. So if
        -- another publisher has already processed somehow this method call its
        -- status will be returned.
        pubPub <- Subs.getPublicationTwoWayCommit schmPub
                bizMethodCall shardIdDst
        pubDst <- Subs.getPublicationTwoWayCommit schmDst
                 bizMethodCall shardIdDst
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
                                shardIdPub
                                shardIdDst
                        -- Publications log on destination.
                        pubIdDst <- Subs.insertPublication
                                schmDst
                                subId
                                bizMethodCall
                                shardIdPub
                                shardIdDst
                        return (pubIdPub, pubIdDst)
                -- When destination commits phase 1 and an error occurs just
                -- before publisher commits.
                (Nothing, (Just (pubIdDst, pubShardIdDst, False))) -> do
                        -- Check that the destination does not have this method
                        -- call on its publish log but from another publisher.
                        if shardIdPub /= pubShardIdDst
                                then do
                                        -- Not supported right now!
                                        error $ "<1>Cyclic publication: " ++ (show (pubPub, pubDst))
                                else do
                                        pubIdPub <- Subs.insertPublication
                                                schmPub
                                                subId
                                                bizMethodCall
                                                shardIdPub
                                                shardIdDst
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
                ((Just (pubIdPub, pubShardIdPub, False)), (Just (pubIdDst, pubShardIdDst, False))) -> do
                        -- Check that both publication logs entries are from the
                        -- same publishers.
                        if pubShardIdPub /= pubShardIdDst
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
                Schema.beginTransactionRWSerial schmPub
                rowsUpdated <- withConnectionRW
                        connDst
                        (\schmDst -> do
                                -- Serializable transaction on destination.
                                -- No retying!
                                Schema.beginTransactionRWSerial schmDst
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
                                Schema.commit schmDst
                                -- Right: No exceptions caught.
                                return (Right rowsUpdated)
                        )
                -- If something fails here (before publisher commits) the
                -- two-way-commit resolves it. The problem is that the business
                -- method is replicated on both origin and destination but only
                -- the destination has two way commits.
                Schema.commit schmPub
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
                Schema.beginTransactionRWSerial schm
                bizCalls <- Subs.getBizMethodsNotReplicatedFirst schm limit
                replicationsIds <- mapM
                        (\bmc -> finally
                                (do
                                        let bizMethodId = Subs.bizMethodCallBusinessMethodId
                                                bmc
                                        let bizMethodCallId = Subs.bizMethodCallId
                                                bmc
                                        Schema.setBizMethodIds
                                                schm
                                                (bizMethodId, bizMethodCallId)
                                        Subs.replicateBizMethodCall schm bmc
                                        ans <- Subs.insertPublicationReplication
                                                schm bmc
                                        Schema.notifyBizMethodCall
                                                schm
                                        return ans
                                )
                                (Schema.resetBizMethodIds schm)
                        )
                        bizCalls
                Schema.commit schm
                -- Right: No exceptions caught.
                return (Right replicationsIds)
        )
