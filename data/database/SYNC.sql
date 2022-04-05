# Sync.
--------------------------------------------------------------------------------

-- How to sync the catalog's metadata.

-- UNDEFINE
--------------------------------------------------------------------------------

-- TABLES
DROP TABLE IF EXISTS sync.subscriptions_publications_replications RESTRICT;
DROP TABLE IF EXISTS sync.subscriptions_publications RESTRICT;
DROP TABLE IF EXISTS sync.subscriptions_shards RESTRICT;
DROP TABLE IF EXISTS sync.subscriptions_business_methods RESTRICT;
DROP TABLE IF EXISTS sync.subscriptions RESTRICT;
DROP TABLE IF EXISTS sync.shards RESTRICT;
-- FUNCTIONS.
DROP FUNCTION IF EXISTS sync.data_module_version_id();
DROP FUNCTION IF EXISTS sync.data_module_id();
-- SEQUENCES
--
-- TYPES
--
-- SCHEMA
DROP SCHEMA IF EXISTS sync RESTRICT;

--------------------------------------------------------------------------------

CREATE SCHEMA sync;

--------------------------------------------------------------------------------

-- Which module this file represents on "catalog.data_module".
CREATE OR REPLACE FUNCTION sync.data_module_id () RETURNS BIGINT AS $$
        SELECT 2::BIGINT;
$$ LANGUAGE SQL
        IMMUTABLE
        -- Option LEAKPROOF can only be set by the superuser.
        -- Can't use it with Amazon RDS.
        -- Functions which do not take arguments or which are not passed any
        -- arguments from the security barrier view or table do not have to be
        -- marked as leakproof to be executed before security conditions.
        -- LEAKPROOF
        SECURITY DEFINER
        PARALLEL SAFE
        SET search_path = public
;

-- The actual version of this module stored on "catalog.data_module_version_id".
-- Do not assume that this IDs are in chronological order, use table
-- "catalog.data_modules_versions" to sort.
CREATE OR REPLACE FUNCTION sync.data_module_version_id () RETURNS BIGINT AS $$
        -- CHANGE ME!!!!!!!!!!!!!!!!!!!!!!!!!!
        SELECT (-9223372036854775808)::BIGINT;
$$ LANGUAGE SQL
        IMMUTABLE
        -- Option LEAKPROOF can only be set by the superuser.
        -- Can't use it with Amazon RDS.
        -- Functions which do not take arguments or which are not passed any
        -- arguments from the security barrier view or table do not have to be
        -- marked as leakproof to be executed before security conditions.
        -- LEAKPROOF
        SECURITY DEFINER
        PARALLEL SAFE
        SET search_path = public
;

--------------------------------------------------------------------------------

-- The origin of the business method call is centralized here.

-- TODO: CREATE TABLE meta.business_methods_calls_origins (

/*
        business_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.business_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,

        origin_shard_id BIGINT NOT NULL
                CONSTRAINT "origin_shard_id_check"
                CHECK (origin_shard_id >= 0),
*/

--------------------------------------------------------------------------------

CREATE TABLE sync.shards (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Non-Automatic ID that must be able to be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE,
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
--
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
--
) WITHOUT OIDS;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Subscriptions.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- A subscription starts with only a name, it has business methods and
-- (origin,destination) tuples that are stored on two other tables.
-- It's like and Observer or Publish/Subscribe pattern, designed to be pull or
-- push and through one to many hops (or shards).
CREATE TABLE sync.subscriptions (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
--
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(display_name)
) WITHOUT OIDS;

-- TODO: Flag the last time this subscriptions checked for news.
CREATE TABLE sync.subscriptions_updated (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        subscriptions_id BIGINT NOT NULL
                REFERENCES sync.subscriptions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
        publisher_shard_id BIGINT NOT NULL
                CONSTRAINT "publisher_shard_id_check"
                CHECK ( publisher_shard_id >= 0 ),
-- Individual data: ************************************************************
--******************************************************************************
        last_business_method_creation_time TIMESTAMP WITH TIME ZONE NOT NULL,
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(subscriptions_id)
) WITHOUT OIDS;

-- The business methods of a subscription.
-- A business method can only belong to one subscription, so we always know from
-- which subscription a business method was published and helps avoiding a
-- situation were a business method is published twice.
-- But, it's a fucking chaos the process of moving a busineess method to another
-- subscription (First change the unique restriction of this tables, update the
-- business method, update sync.subscriptions_publications and restore the
-- unique restriction).
-- Is it worth ??? By doing this the application layer has no flexibility
-- besides choosing which subscriptions to join and from which publisher
-- (changing subscriptions_shards) or maybe prioritizing them. The definition of
-- the subscription is up to the data layer and I think this is the key point.
-- This is a definition of the blocks of business method that go together,
-- something that is defined by the shema. If you want to modify it, modify the
-- schema!
CREATE TABLE sync.subscriptions_business_methods (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        business_methods_id BIGINT NOT NULL
                REFERENCES catalog.business_methods (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        subscriptions_id BIGINT NOT NULL
                REFERENCES sync.subscriptions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(business_methods_id),
        -- For the foreign key of "sync.subscriptions_publications".
        UNIQUE(business_methods_id, subscriptions_id)
) WITHOUT OIDS;

-- TODO: Create a table sync.subscriptions_dependencies.
-- TODO: I think this must be created on catalog.business_methods_dependencies.
-- Right now the business layer must manage subscriptions dependencies. When
-- creating sync.subscriptions_shards rows be careful to include all its
-- dependencies.

-- The business methods originated from which shard a shard want to be
-- subscribed to. The (origin,destination) tuples of a subscription.
-- Who makes the publishing is not important here, only that someone wants to
-- notify or be notified about something. The publisher is stored on
-- sync.subscriptions_publications.
CREATE TABLE sync.subscriptions_shards (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        subscriptions_id BIGINT NOT NULL
                REFERENCES sync.subscriptions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
        origin_shard_id BIGINT NOT NULL
                CONSTRAINT "origin_shard_id_check"
                CHECK ( origin_shard_id >= 0 ),
        destination_shard_id BIGINT NOT NULL
                CONSTRAINT "destination_shard_id_check"
                CHECK ( destination_shard_id >= 0 ),
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(subscriptions_id, origin_shard_id, destination_shard_id),
        CONSTRAINT "subscriptions_shards_check"
        CHECK (NOT (origin_shard_id = destination_shard_id))
) WITHOUT OIDS;

/* TODO: Create this on a third subscriptions_something table:
        time_range tstzrange NOT NULL
                DEFAULT '[now,infinity)',
*/

-- The publication log.
-- Publication is a different process than replication.
-- Publication of the same business method call can appear more than once but
-- with different destinations.
CREATE TABLE sync.subscriptions_publications (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        business_methods_id BIGINT NOT NULL
                REFERENCES catalog.business_methods (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        business_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.business_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        subscriptions_id BIGINT NOT NULL
                REFERENCES sync.subscriptions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
        publisher_shard_id BIGINT NOT NULL
                CONSTRAINT "publisher_shard_id_check"
                CHECK ( publisher_shard_id >= 0 ),
        origin_shard_id BIGINT NOT NULL
                CONSTRAINT "origin_shard_id_check"
                CHECK ( origin_shard_id >= 0 ),
        destination_shard_id BIGINT NOT NULL
                CONSTRAINT "destination_shard_id_check"
                CHECK ( destination_shard_id >= 0 ),
-- Individual data: ************************************************************
--******************************************************************************
        two_way_commit BOOLEAN NOT NULL
                DEFAULT FALSE,
--Constraints: *****************************************************************
--******************************************************************************
        -- A shard can publish the same method call to multiple destinations.
        UNIQUE(business_methods_calls_id, destination_shard_id),
        -- Make sure we are all referencing the same business method.
        FOREIGN KEY (business_methods_calls_id, business_methods_id)
                REFERENCES meta.business_methods_calls (id, business_methods_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- Make sure we are all referencing the same business method origin.
        FOREIGN KEY (business_methods_calls_id, origin_shard_id)
                REFERENCES meta.business_methods_calls (id, origin_shard_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- Make sure we are all referencing the same subscription business method.
        FOREIGN KEY (business_methods_id, subscriptions_id)
                REFERENCES sync.subscriptions_business_methods (business_methods_id, subscriptions_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- Make sure we are all referencing the same subscription shards.
        FOREIGN KEY (subscriptions_id, origin_shard_id, destination_shard_id)
                REFERENCES sync.subscriptions_shards (subscriptions_id, origin_shard_id, destination_shard_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- The FK of sync.subscriptions_shards already checks origin_shard_id
        -- and destination_shard_id.
        -- Row publisher_shard_id can be equal to destination_shard_id if a
        -- shard starts accepting method calls that it was just proxying.
        CONSTRAINT "subscriptions_publications_check"
        CHECK ( NOT origin_shard_id = destination_shard_id )
) WITHOUT OIDS;

CREATE TABLE sync.subscriptions_publications_replications (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        business_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.business_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
        destination_shard_id BIGINT NOT NULL
                CONSTRAINT "destination_shard_id_check"
                CHECK ( destination_shard_id >= 0 ),
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        -- Make sure we are all referencing the same subscription publication.
        FOREIGN KEY (business_methods_calls_id, destination_shard_id)
                REFERENCES sync.subscriptions_publications (business_methods_calls_id, destination_shard_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE
) WITHOUT OIDS;
