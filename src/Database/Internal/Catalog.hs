{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- The catalog is structured first by Data Modules. This data modules are named
-- after the underlying database schema, this is just a logical separation that
-- may later help organizing updates/upgrades, permissions, etc.
--
-- Every schema can have many versions as it evolves. The actual version is
-- wired on a special function of the schema that is updated as the schema
-- changes.
--
-- A data module has methods that are independent of the version. For example
-- a module may have methods createBoy, createGirl and createPerson were the
-- latter takes no sex parameter but only createPerson is available on the
-- actual versions. Or maybe the formers on the actual versions just reference
-- different tables.
-- This way is easier to evolve the method by just flagging them as available on
-- a versions or chaging the table they use.
--
-- Finally, to use a Data Method you need the module it belongs, its name and
-- the actual version of the module on the database!

module Database.Internal.Catalog (

        -- * Applications.

          getApplicationId

        -- * Business method - data methods.

        , getMethod

        -- * Business layer.

        -- ** Business methods.

        , getBizMethodId

        -- ** Business versions.

        , getBizVersionId

        -- * Data layer.

        , DataMethodVerb (..)
        , getDataMethod

        -- ** Data modules.

        , getDataModuleId

        -- *** Data modules methods.

        , getDataModuleMethodId

        -- *** Data modules versions.

        , getDataModuleVersionId

) where

--------------------------------------------------------------------------------

-- base.
import Data.String (fromString)
-- Package: postgresql-simple.
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Types as PSQLTypes
-- Package: vector.
import qualified Data.Vector as Vector

-- Aplication layer.
--------------------------------------------------------------------------------

-- | Look for the application ID by name.
-- The application must exists and its name be unique.
getApplicationId :: PSQL.Connection -> String -> IO Integer
getApplicationId psql applicationName = do
        [(PSQL.Only methodId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.applications "
                        <> " WHERE display_name = ? "
                        <> " ; "
                )
                (PSQL.Only applicationName)
        return methodId

--------------------------------------------------------------------------------

getMethod :: PSQL.Connection
          -- ^ The biz method.
          -> Integer
          -- ^ The biz version.
          -> Integer
          -- ^ The data method version.
          -> Integer
          -- ^ ID.
          -> IO Integer
getMethod psql bizMethodId bizVersionId dataMethodId = do
        -- The biz method data method  must exist.
        [(PSQL.Only methodVersionId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.methods "
                        <> " WHERE "
                                <> " business_methods_id = ? "
                                <> " AND "
                                <> " business_versions_id = ? "
                                <> " AND "
                                <> " data_methods_id = ? "
                )
                (bizMethodId, bizVersionId, dataMethodId)
        return methodVersionId

-- Business layer.
--------------------------------------------------------------------------------

-- | Look for the business method ID by name.
-- The business method must exists and its name be unique.
getBizMethodId :: PSQL.Connection -> String -> IO Integer
getBizMethodId psql methodName = do
        [(PSQL.Only methodId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.business_methods "
                        <> " WHERE display_name = ? "
                        <> " ; "
                )
                (PSQL.Only methodName)
        return methodId

-- | Look for the business version ID by providing a numeric version.
-- The business version number must exists.
getBizVersionId :: PSQL.Connection -> [Integer] -> IO Integer
getBizVersionId psql bizVersionNumber = do
        [(PSQL.Only bizVersionId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.business_versions "
                        <> " WHERE version = ? "
                        <> " ; "
                )
                (PSQL.Only $ PSQLTypes.PGArray bizVersionNumber)
        return bizVersionId

-- | Look for the business version name by providing an ID.
-- The business version ID must exists.
_getBizVersionName :: PSQL.Connection -> Integer -> IO (Maybe String)
_getBizVersionName psql bizVersionId = do
        [(PSQL.Only versionName)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT display_name "
                        <> " FROM catalog.business_versions "
                        <> " WHERE id = ? "
                )
                (PSQL.Only bizVersionId)
        return versionName

-- | Look for the business version number by providing an ID.
-- The business version ID must exists.
_getBizVersionNumber :: PSQL.Connection -> Integer -> IO [Integer]
_getBizVersionNumber psql bizVersionId = do
        [(PSQL.Only (PSQLTypes.PGArray bizVersionNumber))] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT version "
                        <> " FROM catalog.business_versions "
                        <> " WHERE id = ? "
                        <> " ; "
                )
                (PSQL.Only bizVersionId)
        return bizVersionNumber

--------------------------------------------------------------------------------

data DataMethodVerb = INSERT | UPDATE | DELETE
        deriving Show

getDataMethod :: PSQL.Connection
              -- ^ The data method module name.
              -> String
              -- ^ The data method name.
              -> String
              -- ^ The data method table name.
              -> String
              -- ^ The data method action.
              -> DataMethodVerb
              -- ^ ID.
              -> IO Integer
getDataMethod psql moduleName methodName table verb = do
        -- The data module name must exist.
        moduleId <- getDataModuleId psql moduleName
        -- The data module method name must exist.
        moduleMethodId <- getDataModuleMethodId psql moduleId methodName
        -- The data module must be set on the database schema.
        moduleVersionId <- getDataModuleVersionId psql moduleName
        -- The data method version must exist.
        [(PSQL.Only (dataMethodId::Integer))] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.data_methods "
                        <> " WHERE "
                                <> " data_modules_id = ? "
                                <> " AND "
                                <> " data_modules_methods_id = ? "
                                <> " AND "
                                <> " data_modules_versions_id = ? "
                                <> " AND "
                                <> " table_name = ? "
                                <> " AND "
                                <> " verb = ? "
                                <> " AND "
                                <> " enabled "
                        <> " ; "
                )
                (
                          moduleId
                        , moduleMethodId
                        , moduleVersionId
                        , table
                        , (show verb)
                )
        return dataMethodId

--------------------------------------------------------------------------------

-- | Look for the data module ID by name.
-- The data module must exists and its name be unique.
getDataModuleId :: PSQL.Connection -> String -> IO Integer
getDataModuleId psql moduleName = do
        [(PSQL.Only moduleId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.data_modules "
                        <> " WHERE display_name = ? "
                        <> " ; "
                )
                (PSQL.Only moduleName)
        return moduleId

-- | Look for the data method ID, of the provided data module, by name.
-- The data module method name must exists and its name be unique for the
-- provided module.
getDataModuleMethodId :: PSQL.Connection -> Integer -> String -> IO Integer
getDataModuleMethodId psql moduleId methodName = do
        [(PSQL.Only dataModuleMethodId)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT id "
                        <> " FROM catalog.data_modules_methods "
                        <> " WHERE "
                                <> " data_modules_id = ? "
                                <> " AND "
                                <> " display_name = ? "
                )
                (moduleId, methodName)
        return dataModuleMethodId

-- | Look for the actual data module version of the provided data module.
-- The data module name must correspond with the schema name and have its
-- corresponding data_module_version_id() function updated.
getDataModuleVersionId :: PSQL.Connection -> String -> IO Integer
getDataModuleVersionId psql moduleName = do
        [(PSQL.Only moduleVersionId)] <- PSQL.query_ psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT " <> (fromString moduleName) <> ".data_module_version_id() "
                        <> " ; "
                )
        return moduleVersionId

-- | Look for the data module version number by providing an ID.
_getDataModuleVersionNumber :: PSQL.Connection -> Integer -> IO [Integer]
_getDataModuleVersionNumber psql moduleVersionId = do
        [(PSQL.Only moduleVersionNumber)] <- PSQL.query psql
                -- Table rarely updated, SELECT without serialization concerns.
                (" SELECT version "
                        <> " FROM catalog.data_modules_versions "
                        <> " WHERE id = ? "
                        <> " ; "
                )
                (PSQL.Only moduleVersionId)
        return (Vector.toList moduleVersionNumber)
