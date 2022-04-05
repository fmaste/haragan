-- The CATALOG is a higher entity, like the Linux kernel, like GHC's runtine
-- system or god. It has its own type of DSGIDs and it's like the API/ABI of the
-- data access layer (Or DAL for short).

-- Here it is defined what individual actions/methods/verbs are available or can
-- be run on which table. For example table COUNTRIES may only accept INSERTS.
-- Also the business methods define which table/data methods can be run together
-- on a single transaction or group of indivisible data actions.
-- All this helps syncing between databases, creating backups and restores, etc.

-- The responsability of tracking which data_versions is actually available
-- resides here but not the one of the business version. Only the application
-- layer knowns which business versions is using or requesting. This is mostly
-- because the business version can never be enforced here, only flagged as
-- deprecated or unavailable.

-- The shard ID used to create the DSGIDs (distributed sharded global IDs) used
-- in this tables is always 0. This can't be changed.

--------------------------------------------------------------------------------

-- First we have the modules in "data_modules" where we store only the unique
-- names of each individual schema. The idea is for each name to be of the form
-- "A.B.C". Like for example "person.car.new" when relating a car to a person.

-- Each of this modules has its associated version numbers (just the numbers) in
-- table "data_modules_versions" and its method names (just the names) in
-- "data_modules_methods".

-- The description of each method (and the relationship module-version-method)
-- resides in "data_methods" were we have the table name and the type of
-- operation (INSERT, DELETE, UPDATE). Like this a method can freely change
-- action verb or table name used between versions. The method can also be
-- disabled!

-- The business layer is composed of methods (on "business_methods" just by
-- name) and versions (on "business_versions" just by number). On table
-- "business_methods_versions" you have the method-version relationhip.

-- The table that groups all of this together is "methods" with the
-- "data_methods" and "business_methods_versions". So, for a Given business
-- method on a given version you have the allowed data methods also of a given
-- data version.

--------------------------------------------------------------------------------

-- TABLES
DROP TABLE IF EXISTS catalog.applications RESTRICT;
DROP TABLE IF EXISTS catalog.methods RESTRICT;
--TODO: DROP TABLE IF EXISTS catalog.business_methods_versions_dependencies RESTRICT;
DROP TABLE IF EXISTS catalog.business_versions RESTRICT;
DROP TABLE IF EXISTS catalog.business_methods RESTRICT;
--TODO: DROP TABLE IF EXISTS catalog.data_updates_versions RESTRICT;
--TODO: DROP TABLE IF EXISTS catalog.data_updates RESTRICT;
DROP TABLE IF EXISTS catalog.data_methods RESTRICT;
DROP TABLE IF EXISTS catalog.data_modules_versions RESTRICT;
DROP TABLE IF EXISTS catalog.data_modules_methods RESTRICT;
DROP TABLE IF EXISTS catalog.data_modules RESTRICT;
-- FUNCTIONS
DROP FUNCTION IF EXISTS catalog.data_module_version_id();
DROP FUNCTION IF EXISTS catalog.data_module_id();
DROP FUNCTION IF EXISTS catalog.global_id_generator();
-- SEQUENCES
--
-- TYPES
DROP TYPE IF EXISTS catalog.DATA_METHOD_VERB;
-- SCHEMA
DROP SCHEMA IF EXISTS catalog RESTRICT;

--------------------------------------------------------------------------------

CREATE SCHEMA catalog;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Utils.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Which module this file represents on "catalog.data_module".
CREATE OR REPLACE FUNCTION catalog.data_module_id () RETURNS BIGINT AS $$
        SELECT 0::BIGINT;
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
CREATE OR REPLACE FUNCTION catalog.data_module_version_id () RETURNS BIGINT AS $$
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

-- This IDs depend only on the time created.
CREATE OR REPLACE FUNCTION catalog.global_id_generator (OUT ans BIGINT) AS $$
DECLARE
        shard_id BIGINT := 0;
        seq_id BIGINT := 0;
BEGIN
        SELECT public.global_id_generator( shard_id, seq_id ) INTO ans;
END;
$$ LANGUAGE PLPGSQL
        VOLATILE
        -- Option LEAKPROOF can only be set by the superuser.
        -- Can't use it with Amazon RDS.
        -- Functions which do not take arguments or which are not passed any
        -- arguments from the security barrier view or table do not have to be
        -- marked as leakproof to be executed before security conditions.
        -- LEAKPROOF
        SECURITY DEFINER
        -- Allowed to run in parallel, without running in parallel the 10 bits
        -- of the seq_id are useless.
        PARALLEL SAFE
        SET search_path = public
;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Data methods.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- The data modules / packages / schemas available.

CREATE TABLE catalog.data_modules (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        -- Name can be NULL or must be unique.
        UNIQUE(display_name) NOT DEFERRABLE
) WITHOUT OIDS;

-- The available methods on the schema layer to run on data layer.

-- When running a data method first it is looked here by name and then by
-- schema version on "catalog.data_methods".
-- This table is only referenced by catalog.data_methods and
-- catalog.data_methods_calls.

CREATE TABLE catalog.data_modules_methods (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        -- The namespace or logical group of tables.
        data_modules_id BIGINT NOT NULL
                REFERENCES catalog.data_modules (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        -- Name can be NULL or must be unique for a module.
        UNIQUE(data_modules_id, display_name) NOT DEFERRABLE,
        -- For the foreign key of "data_methods".
        UNIQUE(id, data_modules_id) NOT DEFERRABLE
) WITHOUT OIDS;

-- Data modules have versions.

CREATE TABLE catalog.data_modules_versions (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        data_modules_id BIGINT NOT NULL
                REFERENCES catalog.data_modules (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        -- List of non-negative Integers, for example '{2,64,891}' version.
        version INTEGER[] NOT NULL
                -- Can't create a list of non-negative integers because using
                -- SERIAL[] gives "ERROR:  array of serial is not implemented"
                -- as an error. So we check it with a constraint.
                CONSTRAINT "version_check"
                CHECK ( position('-' in array_to_string( version, '.' )) = 0 ),
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(data_modules_id, version) NOT DEFERRABLE,
        -- Name can be NULL or must be unique for a module.
        UNIQUE(data_modules_id, display_name) NOT DEFERRABLE,
        -- For the foreign key of "data_methods".
        UNIQUE(id, data_modules_id) NOT DEFERRABLE
) WITHOUT OIDS;

-- Every data method can only INSERT, UPDATE OR DELETE a single database table.
-- Here the method exists only by name independent of the API version.

CREATE TYPE catalog.DATA_METHOD_VERB AS ENUM ( 'INSERT', 'UPDATE', 'DELETE' );

-- A data_method can belong to one or many schema versions and have different
-- logic between versions. During versions not much may change besides name or
-- simple internal logic, but if the name changes here it is stored which name
-- was used when the data method was available.

-- Also will be used to upgrade/downgrade data method calls between schema
-- versions so when syncing multiple servers no all of them need to have the
-- same version (TODO, subject to "Is this even possible?").

CREATE TABLE catalog.data_methods (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        -- Just to be sure we are referencing the same module.
        data_modules_id BIGINT NOT NULL
                REFERENCES catalog.data_modules (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        data_modules_methods_id BIGINT NOT NULL
                REFERENCES catalog.data_modules_methods (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        data_modules_versions_id BIGINT NOT NULL
                REFERENCES catalog.data_modules_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        -- Postgre's internal type NAME is not portable, using TEXT instead.
        table_name TEXT NOT NULL,
        verb catalog.DATA_METHOD_VERB NOT NULL,
        enabled BOOLEAN NOT NULL,
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(data_modules_methods_id, data_modules_versions_id) NOT DEFERRABLE,
        -- Make sure we are all referencing the same data_modules_id.
        FOREIGN KEY (data_modules_methods_id, data_modules_id)
                REFERENCES catalog.data_modules_methods (id, data_modules_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        -- Make sure we are all referencing the same data_modules_id.
        FOREIGN KEY (data_modules_versions_id, data_modules_id)
                REFERENCES catalog.data_modules_versions (id, data_modules_id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE
) WITHOUT OIDS;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Business methods.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- The available methods on the business layer.
-- Here the method exists only by name independent of the API version.
-- During versions not much may change besides simple internal logic.

CREATE TABLE catalog.business_methods (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        -- Name can be NULL or must be unique.
        UNIQUE(display_name) NOT DEFERRABLE
) WITHOUT OIDS;

-- Set the version for the business logic.
-- The one actually in use is given by the business layer.

-- The business version must already be "registered" on the shema to run
-- correctly because this table belongs to the catalog.
-- When a new business version is released it must be registered here. It could
-- be when replicating the database or when installing the executable binary.

CREATE TABLE catalog.business_versions (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        -- List of non-negative Integers, for example '{2,64,891}' version.
        version INTEGER[] NOT NULL
                -- Can't create a list of non-negative integers because using
                -- SERIAL[] gives "ERROR:  array of serial is not implemented"
                -- as an error. So we check it with a constraint.
                CONSTRAINT "version_check"
                CHECK ( position('-' in array_to_string( version, '.' )) = 0 ),
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(version) NOT DEFERRABLE,
        -- Name can be NULL or must be unique.
        UNIQUE(display_name) NOT DEFERRABLE
) WITHOUT OIDS;

-- TODO:
-- Table to hold data of a DAG (directed acyclc graph) with the dependencies
-- of the methods.
CREATE TABLE catalog.business_methods_versions_dependencies (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
        -- When I inserted it on the table by using NOW.
        creation_time TIMESTAMP WITH TIME ZONE NOT NULL
                DEFAULT transaction_timestamp(),
        -- To use as internal description if it has none or as name to show.
        display_name TEXT DEFAULT NULL
                CONSTRAINT "display_name_check"
                CHECK (display_name IS NULL OR NOT display_name = ''),
-- References: *****************************************************************
--******************************************************************************
        parent BIGINT NOT NULL
                REFERENCES catalog.business_methods_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        child BIGINT NOT NULL
                REFERENCES catalog.business_methods_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        distance INT NOT NULL
                CONSTRAINT "distance_check"
                CHECK (distance > 0),
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(parent, child) NOT DEFERRABLE
) WITHOUT OIDS;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- API.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- business_methods (versions) <-> data_modules_methods (version)

CREATE TABLE catalog.methods (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        business_versions_id BIGINT NOT NULL
                REFERENCES catalog.business_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        data_methods_id BIGINT NOT NULL
                REFERENCES catalog.data_methods (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE
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
-- Applications.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

CREATE TABLE catalog.applications (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        -- Name can be NULL or must be unique.
        UNIQUE(display_name) NOT DEFERRABLE
) WITHOUT OIDS;

-- TODO: Which business methods an application can run!

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Updates.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Update not upgrade. This is intended to be used for incremental updates that
-- make the schema evolve. An upgrade would mean reinstalling a server and
-- schema from the ground up or using a special script or business layer program
-- that works as if applying many updates at once (Should we named this a
-- Service pack ??? Please no!).

/* How to upgrade an async-multi-master database group:

 - Option 1) TOI (Total order isolation):
 - - a) If asynchronous replication:
 - - - 1) Switch to synchronous for every participating database.
 - - b) If synchronous replication:
 - - - 1) Send all DDL commands to run to all databases.
 - - - 2) When every biz method is replicated and applied, block new commits.
 - - - 3) Apply DDL commands on all databases before accepting new biz methods.
 - Option 2) RSU (Rolling Schema upgrade):
 - -

Also see: https://en.wikipedia.org/wiki/CAP_theorem

*/

CREATE TABLE catalog.data_updates (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        sql_ddl_commands TEXT NOT NULL
--Constraints: *****************************************************************
--******************************************************************************
) WITHOUT OIDS;

CREATE TABLE catalog.data_updates_versions (
-- Standard columns: ***********************************************************
--******************************************************************************
        -- Automatic internal ID that can be sharded.
        id BIGINT NOT NULL PRIMARY KEY NOT DEFERRABLE
                DEFAULT catalog.global_id_generator(),
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
        data_modules_versions_id BIGINT NOT NULL
                REFERENCES catalog.data_modules_versions (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
        data_updates_id BIGINT NOT NULL
                REFERENCES catalog.data_updates (id) MATCH FULL
                ON DELETE RESTRICT ON UPDATE RESTRICT NOT DEFERRABLE,
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        UNIQUE(data_modules_versions_id) NOT DEFERRABLE
) WITHOUT OIDS;
