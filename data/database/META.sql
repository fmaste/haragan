# Meta.
--------------------------------------------------------------------------------

-- Metada, data about the data. Here we store the atomic operations done on each
-- module-table logically grouped by business methods, an application layer
-- definition.
-- This is useful to synchronize databases, make backups and evolve the schema
-- while doing so.

-- UNDEFINE
--------------------------------------------------------------------------------

-- TABLES
DROP TABLE IF EXISTS meta.methods_calls_versions RESTRICT;
DROP TABLE IF EXISTS meta.data_methods_calls RESTRICT;
DROP TABLE IF EXISTS meta.business_methods_calls_retries RESTRICT;
DROP TABLE IF EXISTS meta.business_methods_calls RESTRICT;
-- FUNCTIONS.
DROP FUNCTION IF EXISTS meta.data_module_version_id();
DROP FUNCTION IF EXISTS meta.data_module_id();
-- SEQUENCES
--
-- TYPES
--
-- SCHEMA
DROP SCHEMA IF EXISTS meta RESTRICT;

--------------------------------------------------------------------------------

CREATE SCHEMA meta;

--------------------------------------------------------------------------------

-- Which module this file represents on "catalog.data_module".
CREATE OR REPLACE FUNCTION meta.data_module_id () RETURNS BIGINT AS $$
        SELECT 1::BIGINT;
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
CREATE OR REPLACE FUNCTION meta.data_module_version_id () RETURNS BIGINT AS $$
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

-- Here we keep a time-ordered log of called business tier methods. Every row
-- must represent a serializable SQL transaction commited on this shard, but if
-- every transaction commited has a row is up to the tier that manages the data
-- or business logic of this shard (of course a missing action makes from 0 to N
-- future rows inserted here invalid).
--
-- Serialization anomaly:
-- The result of successfully committing a group of transactions is inconsistent
-- with all possible orderings of running those transactions one at a time.
--
-- While PostgreSQL's Serializable transaction isolation level only allows
-- concurrent transactions to commit if it can prove there is a serial order of
-- execution that would produce the same effect, it doesn't always prevent
-- errors from being raised that would not occur in true serial execution. In
-- particular, it is possible to see unique constraint violations caused by
-- conflicts with overlapping Serializable transactions even after explicitly
-- checking that the key isn't present before attempting to insert it. This can
-- be avoided by making sure that all Serializable transactions that insert
-- potentially conflicting keys explicitly check if they can do so first. For
-- example, imagine an application that asks the user for a new key and then
-- checks that it doesn't exist already by trying to select it first, or
-- generates a new key by selecting the maximum existing key and adding one. If
-- some Serializable transactions insert new keys directly without following
-- this protocol, unique constraints violations might be reported even in cases
-- where they could not occur in a serial execution of the concurrent
-- transactions.
-- For optimal performance when relying on Serializable transactions for
-- concurrency control, these issues should be considered to avoid retries as
-- much as possible:
-- - Try to reuse a single connection to the database as much as possible.
-- - Don't leave connections idle in transaction longer than necessary.
-- - Don't put more into a single transaction than needed.
-- - Mark transaction as ReadOnly when no wrting is needed.
-- - Eliminate explicit locks, SELECT FOR UPDATE, and SELECT FOR SHARE.
-- - Any data read from a permanent user table not be considered valid until the
--   transaction which read it has successfully committed (Please try not to
--   read anything when writing!!!!!!).
-- - A sequential scan will always necessitate a relation-level predicate lock.
-- - This can result in an increased rate of serialization failures. It may be
-- - helpful to encourage the use of index scans

-- This is the method's parent table, the data methods that belong to this
-- transactions are on table meta.data_methods_calls and the
-- synchronization details are on tables sync.subscriptions(_(.*)?).

CREATE TABLE meta.business_methods_calls (
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
-- Notice that a business method may have data methods from different business
-- versions, the application must check this, can't be done here.
-- Also can't enforce here that a business method must have at least one data
-- method, again this is responsibility of the application layer.
--
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        -- The application layer must update this value when it finishes.
        -- https://www.postgresql.org/docs/current/functions-info.html#FUNCTIONS-COMMIT-TIMESTAMP
        commit_time TIMESTAMP WITH TIME ZONE
                DEFAULT NULL
--Constraints: *****************************************************************
--******************************************************************************
--
) WITHOUT OIDS;

-- When a business method call was retried because of serialization anomalies.

CREATE TABLE meta.business_methods_calls_retries (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT statement_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT
                DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        -- Fot this to be useful as expected, when retrying the same ID must
        -- be used.
        business_methods_calls_id BIGINT NOT NULL
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

-- The time-ordered actions done that belong to a business method. The operation
-- belongs to a row of a table of a schema and may be an INSERT, UPDATE or
-- DELETE (Like HTTP's POST, PUT and DELETE).
--
-- A biz method can only execute one data method at a time.
--
-- Schema and table OIDs are not stored, the software looks for the
-- corresponding OIDs on the catalog so the schema and table must exists.
-- Of course an INSERT done directly on the database is possible without this
-- extra software check because system tables are not a legal target for a
-- foreign key (PostgreSQL doesn't support foreign key references to system
-- tables. Maybe because these are temporary tables and foreign keys to
-- temporary tables are not allowed).
-- Even though it would not be possible to have a foreign key on these rows,
-- these table will be replicated and OIDs are not globally unique.
--
-- Notice that the data method here is stored in its raw state. The definition
-- of the data method is not stored here. It is up to the software layer to
-- ensure that, for example, if a method is a DELETE operation the content of
-- the call in this table is just an ID.

CREATE TABLE meta.data_methods_calls (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT public.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT statement_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        -- A data method call must not exist without this two. But having the
        -- references here and in meta.methods_calls_versions means that the
        -- database is not normalized.
/*
        methods_id BIGINT NOT NULL
                REFERENCES catalog.methods_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        business_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.business_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
*/
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        -- Actions are INSERT, UPDATE or DELETE (HTTP's POST, PUT and DELETE).
        -- This column is used to identify the row to UPDATE or DELETE.
        row_id BIGINT NOT NULL,
        -- Whe DELETE this column is NULL, else INSERT or UPDATE.
        -- Using JSONB type. Does not preserve white space, object key order and
        -- object key duplicates.
        row_data JSONB
                CONSTRAINT "row_data_check"
                CHECK (
                        row_data IS NULL
                                OR
                        jsonb_typeof( row_data ) = 'object'
/*
                        (
                                NOT row_data = 'null'
                                        AND
                                NOT row_data = 'true'
                                        AND
                                NOT row_data = 'false'
                        )
*/
                )
--Constraints: *****************************************************************
--******************************************************************************
--
) WITHOUT OIDS;

-- The ternary relationship:
--      ( methods_id
--      , business_methods_calls_id
--      , data_methods_calls_id
--      )
-- with cardinality (1,1,N) and the contraint:
--  (data_methods_calls_id) => (business_methods_calls_id)
CREATE TABLE meta.methods_calls (
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
        methods_id BIGINT NOT NULL
                REFERENCES catalog.methods (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        business_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.business_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        data_methods_calls_id BIGINT NOT NULL
                REFERENCES meta.data_methods_calls (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(methods_id, data_methods_calls_id) NOT DEFERRABLE,
        UNIQUE(business_methods_calls_id, data_methods_calls_id) NOT DEFERRABLE,
        UNIQUE(data_methods_calls_id) NOT DEFERRABLE
) WITHOUT OIDS;

--------------------------------------------------------------------------------

/*

TODO: VIEWS

SELECT
          bmcr.id
        , bm.display_name AS method
        , bmcr.creation_time AS time_try
        , bmc.creation_time AS time_ok
        , bmcr.display_name
FROM
          public.business_methods bm
        , meta.business_methods_calls AS bmc
        , meta.business_methods_calls_retries AS bmcr
WHERE
        bm.id = bmc.business_methods_id
        AND
        bmc.id = bmcr.business_methods_calls_id
ORDER BY
        bmcr.creation_time
;

*/
