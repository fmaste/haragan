# Public.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Install and create database.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

/* Install latests PostgreSQL:
> wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
> sudo add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list
> sudo apt-get update
> sudo apt-get --no-install-recommends install postgresql-11
#> sudo apt-get --no-install-recommends install postgresql-contrib-11
*/

--------------------------------------------------------------------------------

/* PostgreSQL hierarchy structure per its documentation:

A PostgreSQL database cluster contains one or more named databases. Users and
groups of users are shared across the entire cluster, but no other data is
shared across databases. Any given client connection to the server can access
only the data in a single database, the one specified in the connection request.

Users of a cluster do not necessarily have the privilege to access every
database in the cluster. Sharing of user names means that there cannot be
different users named, say, joe in two databases in the same cluster; but the
system can be configured to allow joe access to only some of the databases.

A database contains one or more named schemas, which in turn contain tables.
Schemas also contain other kinds of named objects, including data types,
functions, and operators. The same object name can be used in different schemas
without conflict; for example, both schema1 and myschema can contain tables
named mytable. Unlike databases, schemas are not rigidly separated: a user can
access objects in any of the schemas in the database they are connected to, if
they have privileges to do so.

There are several reasons why one might want to use schemas:

- To allow many users to use one database without interfering with each other.

- To organize database objects into logical groups to make them more manageable.

- Third-party applications can be put into separate schemas so they do not
  collide with the names of other objects.

Schemas are analogous to directories at the operating system level, except that
schemas cannot be nested.

*/

/*

We create a sense of nested schemas on the source code by having each schema on
a separate file on a folder hierarchy.

*/

--------------------------------------------------------------------------------

/* Example:

-- Have to call ALTER SYSTEM and DATABASE simultaneosuly to set it
-- This command adds 'intr.db_shard_id = 1' to data_dir/postgresql.auto.conf
-- This parameter is set globally for a server cluster.
ALTER DATABASE dalcon SET intr.shard_id = 1; -- Must be superuser.
ALTER SYSTEM SET intr.shard_id = 1; -- Must be superuser.
SELECT pg_reload_conf();

*/

--------------------------------------------------------------------------------

/* Access:
> sudo -u postgres psql
*/
--------------------------------------------------------------------------------

CREATE DATABASE dalcon
        OWNER postgres
        TEMPLATE DEFAULT
        ENCODING UTF8
        LC_COLLATE 'en_US.UTF-8'
        LC_CTYPE 'en_US.UTF-8'
        TABLESPACE DEFAULT
        ALLOW_CONNECTIONS TRUE
        CONNECTION LIMIT -1
        IS_TEMPLATE FALSE
;

CREATE ROLE dalcon WITH
        NOSUPERUSER
        NOCREATEDB
        NOCREATEROLE
        NOINHERIT
        LOGIN
        NOREPLICATION
        NOBYPASSRLS
        CONNECTION LIMIT -1
        -- ENCRYPTED
        PASSWORD NULL
        -- VALID UNTIL 'timestamp'
        -- IN ROLE role_name [, ...]
        -- IN GROUP role_name [, ...]
        -- ROLE role_name [, ...]
        -- ADMIN role_name [, ...]
        -- USER role_name [, ...]
        -- SYSID uid
;

--------------------------------------------------------------------------------

/* Create system user that will use of the database:
> sudo adduser --system --no-create-home --disabled-password dalcon
> sudo -u postgres psql dalcon
*/

--------------------------------------------------------------------------------

SET search_path TO public;

-- UNDEFINE
--------------------------------------------------------------------------------

-- TABLES
--
-- FUNCTIONS.
DROP FUNCTION IF EXISTS public.global_id_generator(BIGINT, BIGINT);
DROP FUNCTION IF EXISTS public.global_id_generator();
DROP FUNCTION IF EXISTS public.shard_id();
-- SEQUENCES
DROP SEQUENCE public.global_id_sequence;
-- SCHEMA
--

--------------------------------------------------------------------------------

GRANT ALL PRIVILEGES ON SCHEMA public TO dalcon;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public TO dalcon;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO dalcon;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO dalcon;

--------------------------------------------------------------------------------

/*
SSL:
https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.SSL

To connect to a PostgreSQL DB instance over SSL:

- Download the certificate stored at
https://s3.amazonaws.com/rds-downloads/rds-combined-ca-bundle.pem.
- Import the certificate into your operating system.
- Connect to your PostgreSQL DB instance over SSL by appending
sslmode=verify-full to your connection string. When you use sslmode=verify-full,
the SSL connection verifies the DB instance endpoint against the endpoint in the
SSL certificate.

Use the sslrootcert parameter to reference the certificate, for example,
sslrootcert=rds-ssl-ca-cert.pem.
The following is an example of using the psql program to connect to a PostgreSQL
DB instance:
$ psql -h 1.amazonaws.com "sslrootcert=rds-ca-2015-root.pem sslmode=verify-full"

Requiring an SSL Connection to a PostgreSQL DB Instance:

You can require that connections to your PostgreSQL DB instance use SSL by using
the rds.force_ssl parameter. By default, the rds.force_ssl parameter is set to 0
(off). You can set the rds.force_ssl parameter to 1 (on) to require SSL for
connections to your DB instance. Updating the rds.force_ssl parameter also sets
the PostgreSQL ssl parameter to 1 (on) and modifies your DB instance’s
pg_hba.conf file to support the new SSL configuration.

*/

--------------------------------------------------------------------------------

/*
SHOW search_path;
SET search_path TO forecourt;

sudo -u dalcon pg_dump --format=custom --verbose --quote-all-identifiers --serializable-deferrable --file=/tmp/dalcon.dump dalcon
dropdb dalcon
createdb dalcon
"CREATE SCHEMA forecourt"
pg_restore --dbname=dalcon --exit-on-error --file=dalcon/data/dalcon.dump --format=custom --schema=forecourt --no-owner --verbose --no-privileges --single-transaction

Create the definiitions.
pg_restore --data-only --exit-on-error --format=custom --schema=forecourt_posservice --no-owner --verbose --no-privileges --single-transaction --disable-triggers
*/

-- For the exclude constraint of public.restores.
--------------------------------------------------------------------------------

/*
ERROR: permission denied to create extension "btree_gist"
HINT:  Must be superuser to create this extension.
*/
CREATE EXTENSION btree_gist;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Generic function for a Distributed Storage Global ID (DSGID) generator.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Do not assume that the IDs are in cronological order.
CREATE OR REPLACE FUNCTION public.shard_id () RETURNS BIGINT AS $$
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

-- Based on:
-- http://instagram-engineering.tumblr.com/post/10853187575/sharding-ids-at-instagram
-- http://rob.conery.io/2014/05/29/a-better-id-generator-for-postgresql/

-- This system is suitables for:
-- -- Almost 70 years starting from 2019-08-15 14:05:00 UTC.
-- -- 65536 unique shards.
-- -- 128 global IDs per millisecond per shard.
-- This last one is the one to adjust to have more shards (or more time if you
-- pretend to maintain or own a system for 70 years). (Hint: BIGINT supports
-- negative numbers, so -9223372036854775808 could be the new zero ???).
-- The IDs will be ordered first by time, then by shard and last by cycle.
CREATE OR REPLACE FUNCTION public.global_id_generator (
        -- 16 bits after the epoch part (65536 unique shards).
        -- To put this in perspective McDonald's as of 2017 had 37,200 branches.
          IN shard_id BIGINT
        -- The last remaining 7 bits (128 per millisecond for a unique shard).
        -- This means up to 128,000 new IDs per second are possible.
        -- An estimate puts Google on 63,000 searches per second on 2016, this
        -- means for all its servers worldwide and read only queries.
        , IN seq_id BIGINT
        , OUT ans BIGINT
) AS $$
DECLARE
        -- Left-most 41 bits for the time in milliseconds.
        -- Starting with: 2019-08-15 14:05:00 UTC (Epoch seconds: 1565877900).
        -- This gives almost 70 years ( (2^41-1)/1000/60/60/24/365 ).
        -- TODO: Use 42 to achieve almost 140 years ???
        -- NFW, I would be 103 years old, this should be someone else's problem.
        -- `date +%s` plus the 000 for the milliseconds.
        our_epoch BIGINT := 1565877900000;
        now_millis BIGINT;
BEGIN
        -- Epoch comes with milliseonds after the period, see an example:
        -- => SELECT clock_timestamp();
        -- -- 2018-04-20 11:56:05.118521+00
        -- => SELECT EXTRACT(EPOCH FROM '2018-04-20 11:56:05.118521+00'::TIMESTAMP WITH TIME ZONE);
        -- -- 1524225365.11852
        -- => SELECT EXTRACT(EPOCH FROM '2018-04-20 11:56:05.118521+00'::TIMESTAMP WITH TIME ZONE) * 1000;
        -- -- 1524225365118.52
        -- => SELECT FLOOR(EXTRACT(EPOCH FROM '2018-04-20 11:56:05.118521+00'::TIMESTAMP WITH TIME ZONE) * 1000);
        -- -- 1524225365118
        SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp()) * 1000) INTO now_millis;
        ans := (((now_millis - our_epoch)::BIT(64)) << (64-41))::BIGINT;
        ans := ((ans::BIT(64)) | (((shard_id % 65536)::BIT(64)) << (64-41-16)))::BIGINT;
        ans := ((ans::BIT(64)) | ((seq_id % 128)::BIT(64)))::BIGINT;
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
        -- Allowed to run in parallel so clock_timestamp() can return the same
        -- value. The value of seq_id will avoid an ID clash. Without running in
        -- parallel the 7 bits of the seq_id are useless.
        PARALLEL SAFE
        SET search_path = public
;

CREATE SEQUENCE public.id_sequence
INCREMENT BY 1 NO MINVALUE NO MAXVALUE
START WITH 1 CYCLE
OWNED BY NONE
;

CREATE OR REPLACE FUNCTION public.global_id_generator (OUT ans BIGINT) AS $$
DECLARE
        shard_id BIGINT;
        seq_id BIGINT;
BEGIN
        SELECT public.shard_id() INTO shard_id;
        SELECT NEXTVAL( 'public.id_sequence' ) INTO seq_id;
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
-- TODOs
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Manual COPYs or pg_restores.
--
-- Postgre's internal type NAME is not portable, use TEXT for the schema and
-- table name.
--
CREATE TABLE public.data_restores (
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
        schema_name TEXT NOT NULL,
        table_name TEXT NOT NULL,
-- Individual IDs: *************************************************************
--******************************************************************************
        origin_shard_id BIGINT NOT NULL
                CONSTRAINT "origin_shard_id_check"
                CHECK (origin_shard_id >= 0),
        destination_shard_id BIGINT NOT NULL
                CONSTRAINT "destination_shard_id_check"
                CHECK ( destination_shard_id >= 0 ),
        time_range tstzrange NOT NULL,
        --from_time TIMESTAMP WITH TIME ZONE NOT NULL,
        --to_time TIMESTAMP WITH TIME ZONE NOT NULL,
-- Individual data: ************************************************************
--******************************************************************************
--
--Constraints: *****************************************************************
--******************************************************************************
        EXCLUDE USING GiST (
                schema_name WITH =,
                table_name WITH =,
                origin_shard_id WITH =,
                destination_shard_id WITH =,
                --tstzrange(from_time, to_time, '[]') WITH &&
                time_range WITH &&
        ) NOT DEFERRABLE,
        CONSTRAINT "data_restores_check_time"
        CHECK (NOT (origin_shard_id = destination_shard_id))
) WITHOUT OIDS;

-- Processes (or threads) are the mechanics, task is a more conceptual term.
CREATE TABLE public.tasks (
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
                CHECK (display_name IS NULL OR NOT display_name = '')
                NOT DEFERRABLE
-- References: *****************************************************************
--******************************************************************************
--
-- Individual IDs: *************************************************************
--******************************************************************************
--
-- Individual data: ************************************************************
--******************************************************************************
        params JSONB
--Constraints: *****************************************************************
--******************************************************************************
--
) WITHOUT OIDS;

-- TRUNCATE
--------------------------------------------------------------------------------

DELETE FROM public.subscriptions_publications_replications;
DELETE FROM public.subscriptions_publications;
DELETE FROM public.subscriptions_shards;
DELETE FROM public.subscriptions_business_methods;
DELETE FROM public.subscriptions;
DELETE FROM public.data_methods_calls;
DELETE FROM public.business_methods_calls_retries;
DELETE FROM public.business_methods_calls;
DELETE FROM public.data_methods;
DELETE FROM public.business_methods;
DELETE FROM public.business_versions;
DELETE FROM public.schema_versions;

-- Schema versions.
--------------------------------------------------------------------------------

INSERT INTO public.schema_versions (id, creation_time, display_name)
VALUES (454731473824515992,DEFAULT,'0.0');

-- Business versions.
--------------------------------------------------------------------------------

INSERT INTO public.business_versions (id, creation_time, display_name)
VALUES (455923881987803033,DEFAULT,'0.0');

-- Business methods: public.schema.
--------------------------------------------------------------------------------

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.schema.version')
RETURNING id
;

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.schema.version.update')
RETURNING id
;

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.schema.method')
RETURNING id
;

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.business.version')
RETURNING id
;

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.business.method')
RETURNING id
;

--------------------------------------------------------------------------------

INSERT INTO public.business_methods
VALUES (DEFAULT,DEFAULT,'public.subscription.new')
RETURNING id
;

--------------------------------------------------------------------------------

INSERT INTO public.data_methods VALUES
(DEFAULT,DEFAULT,'public.newSchemaVersion','public','schema_versions','INSERT');
RETURNING id
;

INSERT INTO public.data_methods VALUES
(DEFAULT,DEFAULT,'public.newSchemaUpdate','public','schema_updates','INSERT');
RETURNING id
;

INSERT INTO public.data_methods VALUES
(DEFAULT,DEFAULT,'public.newBusinessMethod','public','business_methods','INSERT');
RETURNING id
;

INSERT INTO public.data_methods VALUES
(DEFAULT,DEFAULT,'public.newDataMethod','public','data_methods','INSERT');
RETURNING id
;

-- Subscriptions: public.
--------------------------------------------------------------------------------

INSERT INTO public.subscriptions
VALUES (DEFAULT,DEFAULT,'public');
RETURNING id;

-- Business Method: 'public.businessMethod.new'
INSERT INTO public.subscriptions_business_methods
VALUES (DEFAULT,DEFAULT,DEFAULT,_,_)
RETURNING id;

-- Business Method: 'public.dataMethod.new'
INSERT INTO public.subscriptions_business_methods
VALUES (DEFAULT,DEFAULT,DEFAULT,_,_)
RETURNING id;

-- Business Method: 'public.subscription.new'
INSERT INTO public.subscriptions_business_methods
VALUES (DEFAULT,DEFAULT,DEFAULT,_,_)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,1)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,2)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,3)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,4)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,5)
RETURNING id;

INSERT INTO public.subscriptions_shards
VALUES (DEFAULT,DEFAULT,DEFAULT,_,0,6)
RETURNING id;

--------------------------------------------------------------------------------

GRANT USAGE ON SCHEMA public TO dalcon;
GRANT SELECT, INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO dalcon;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO dalcon;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO dalcon;

GRANT DELETE ON TABLE public.data_methods_calls TO dalcon;
GRANT DELETE ON TABLE public.business_methods_calls TO dalcon;

--------------------------------------------------------------------------------

/*

The standard names for indexes in PostgreSQL are:
(https://stackoverflow.com/a/4108266)

{tablename}_{columnname(s)}_{suffix}

Where the suffix is one of the following:
- pkey for a Primary Key constraint
- key for a Unique constraint
- excl for an Exclusion constraint
- idx for any other kind of index
- fkey for a Foreign key
- check for a Check constraint

Standard suffix for sequences is
- seq for all sequences

*/

-- Which business methods calls are stored and how many!
SELECT COUNT(*), bm.id, bm.display_name, bmc.origin_shard_id
FROM
public.business_methods_calls AS bmc,
public.business_methods AS bm
WHERE
bmc.business_methods_id = bm.id
--AND
--bmc.id NOT IN
--(SELECT business_methods_calls_id FROM public.data_methods_calls)
GROUP BY bm.id, bm.display_name, bmc.origin_shard_id
ORDER BY bm.display_name, bmc.origin_shard_id
;

-- Which data methods calls are stored and how many!
SELECT COUNT(*), dm.id, dm.display_name
FROM
public.data_methods_calls AS dmc,
public.data_methods AS dm
WHERE
dmc.data_methods_id = dm.id
GROUP BY dm.id, dm.display_name
ORDER BY dm.display_name
;

-- Publications: How many, which methods and from where.
SELECT COUNT(*), bm.id, bm.display_name, bmc.origin_shard_id
FROM
public.subscriptions_publications AS sp,
public.business_methods_calls AS bmc,
public.business_methods AS bm
WHERE
sp.business_methods_calls_id = bmc.id
AND
bmc.business_methods_id = bm.id
GROUP BY bm.id, bm.display_name, bmc.origin_shard_id
ORDER BY bm.display_name, bmc.origin_shard_id
;

-- Not Published: How many, which methods and from where.
SELECT COUNT(*), bm.id, bm.display_name, bmc.origin_shard_id
FROM
public.subscriptions_publications AS sp,
public.business_methods_calls AS bmc,
public.business_methods AS bm
WHERE
NOT sp.origin_shard_id = public.shard_id()
AND
sp.business_methods_calls_id = bmc.id
AND
bmc.business_methods_id = bm.id
AND
sp.business_methods_calls_id NOT IN
        (
                SELECT business_methods_calls_id
                FROM public.subscriptions_publications_replications
        )
GROUP BY bm.id, bm.display_name, bmc.origin_shard_id
ORDER BY bm.display_name, bmc.origin_shard_id
;

-- Replications: How many, which methods and from where.
SELECT COUNT(*), bm.id, bm.display_name, bmc.origin_shard_id
FROM
public.subscriptions_publications_replications AS spr,
public.business_methods_calls AS bmc,
public.business_methods AS bm
WHERE
spr.business_methods_calls_id = bmc.id
AND
bmc.business_methods_id = bm.id
GROUP BY bm.id, bm.display_name, bmc.origin_shard_id
ORDER BY bm.display_name, bmc.origin_shard_id
;
