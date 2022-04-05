#!/usr/bin/runhaskell

-- Create and autogen module named PathsCache_PACKAGENAME with the function
-- getDataFileContent :: String :: ByteString
--------------------------------------------------------------------------------

-- Package: base.
import Control.Monad (mapM_, when)
import Data.List (elem)
import System.Environment (getArgs)
import System.IO (withFile, IOMode (WriteMode), Handle, hPutStrLn)
-- Package: ByteString.
import qualified Data.ByteString as BS
-- Package: Cabal.
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Package as Package
import qualified Distribution.PackageDescription as PackageDescription
import qualified Distribution.Simple as Simple
import qualified Distribution.Simple.BuildPaths as BuildPaths
import qualified Distribution.Simple.BuildTarget as BuildTarget
import qualified Distribution.Simple.Flag as Flag
import qualified Distribution.Simple.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Simple.Setup as Setup
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.ComponentRequestedSpec as ComponentRequestedSpec
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.TargetInfo as TargetInfo
import qualified Distribution.Verbosity as Verbosity
-- Package: directory.
import qualified System.Directory as Directory
-- Package: filepath.
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------

main :: IO ()
main = do
        args <- getArgs
        -- A customizable version of defaultMain that also takes the command
        -- line arguments, rather than getting them from the environment.
        Simple.defaultMainWithHooksArgs
                -- Hooks that correspond to a plain instantiation of the
                -- "simple" build system.
                Simple.simpleUserHooks {
                        -- Over-ride this hook to get different behavior during
                        -- build.
                        Simple.buildHook = myBuildHook
                }
                args

-- | First create the autogenerated cache file, then continue with the default
-- build hook.
myBuildHook :: PackageDescription.PackageDescription
            -> LocalBuildInfo.LocalBuildInfo
            -> Simple.UserHooks
            -> Setup.BuildFlags
            -> IO ()
--
-- PackageDescription: This data type is the internal representation of the file
-- pkg.cabal. It contains two kinds of information about the package:
-- information which is needed for all packages, such as the package name and
-- version, and information which is needed for the simple build system only,
-- such as the compiler options and library name.s
--
-- LocalBuildInfo: Once a package has been configured we have resolved
-- conditionals and dependencies, configured the compiler and other needed
-- external programs. The LocalBuildInfo is used to hold all this information.
-- It holds the install dirs, the compiler, the exact package dependencies, the
-- configured programs, the package database to use and a bunch of miscellaneous
-- configure flags. It gets saved and reloaded from a file (dist/setup-config).
-- It gets passed in to very many subsequent build actions.
--
myBuildHook packageDescription localBuildInfo userHooks buildFlags = do
        -- Get the autogen flag status.
        let maybeBool = PackageDescription.lookupFlagAssignment
                -- Flag names are all normalized to lower-case.
                (PackageDescription.mkFlagName "usepathscache")
                (LocalBuildInfo.flagAssignment localBuildInfo)
        -- The autogen file was requested by turning on its correponding flag?
        case maybeBool of
                Nothing -> return ()
                (Just False) -> return ()
                -- Flag is defined and on, look for the actual build targets.
                (Just True) -> do
                        -- Get build targets from the arguments of the build
                        -- command currently being executed.
                        targetInfos <- BuildTarget.readTargetInfos
                                (Flag.fromFlagOrDefault
                                        Verbosity.normal
                                        (Setup.buildVerbosity buildFlags)
                                )
                                packageDescription
                                localBuildInfo
                                (Setup.buildArgs buildFlags)
                        -- For every build target that we are going to build
                        -- check that it is really enabled and create the
                        -- autogenerated cache file.
                        mapM_
                                (\targetInfo -> when
                                        -- Check that the target's component is
                                        -- buildable and requested.
                                        (isTargetEnabled
                                                localBuildInfo targetInfo
                                        )
                                        (myBuildTargetHook
                                                packageDescription
                                                localBuildInfo
                                                targetInfo
                                        )
                                )
                                targetInfos
        -- Now run default build hook.
        (Simple.buildHook Simple.simpleUserHooks)
                packageDescription
                localBuildInfo
                userHooks
                buildFlags

-- | A component is enabled if it is BOTH buildable and requested. (Once we
-- have a LocalBuildInfo, whether or not a component is enabled is known.)
-- A component is buildable if, after resolving flags and conditionals, there
-- is no "buildable: False" property in it. This is a static property that
-- arises from the Cabal file and the package description flattening; once we
-- have a PackageDescription buildability is known.
-- A component is requested if a user specified, via a the flags and arguments
-- passed to configure, that it should be built. E.g., --enable-tests or
-- --enable-benchmarks request all tests and benchmarks, if they are provided.
-- What is requested can be read off directly from ComponentRequestedSpec. A
-- requested component is not always buildable; e.g., a user may
-- --enable-tests but one of the test suites may have buildable: False.
isTargetEnabled :: LocalBuildInfo.LocalBuildInfo
                -> TargetInfo.TargetInfo
                -> Bool
isTargetEnabled localBuildInfo targetInfo =
        let component = TargetInfo.targetComponent targetInfo
            componentRequestedSpec = LocalBuildInfo.componentEnabledSpec
                        localBuildInfo
        in ComponentRequestedSpec.componentEnabled
                componentRequestedSpec
                component

myBuildTargetHook :: PackageDescription.PackageDescription
                  -> LocalBuildInfo.LocalBuildInfo
                  -> TargetInfo.TargetInfo
                  -> IO ()
myBuildTargetHook packageDescription localBuildInfo targetInfo = do
        let component = TargetInfo.targetComponent targetInfo
        let componentBuildInfo = LocalBuildInfo.componentBuildInfo component
        let autogenModuleName = ModuleName.fromString $
                autogenModuleNameStr packageDescription
        when
                -- The file to generate, exists on 'autoge-modules' of the Cabal
                -- description for this component ?
                (elem
                        autogenModuleName
                        (BuildInfo.autogenModules componentBuildInfo)
                )
                -- Create the file on this component autogen modules directory.
                (do
                        -- Get the autogen modules directory that is exclusive
                        -- to this component as create it if not already exists.
                        let componentLocalBuildInfo =
                                TargetInfo.targetCLBI targetInfo
                        let componentAutogenModulesDir =
                                BuildPaths.autogenComponentModulesDir
                                        localBuildInfo
                                        componentLocalBuildInfo
                        Directory.createDirectoryIfMissing
                                True
                                componentAutogenModulesDir
                        -- Convert a module name to a file path, but without any
                        -- file extension. For example:
                        --      toFilePath (fromString "A.B.C") = "A/B/C"
                        let autogenModulePath =
                                ModuleName.toFilePath autogenModuleName
                        -- Get the complete autogen module location and create
                        -- the file with its content.
                        let filePath =
                                componentAutogenModulesDir ++ "/" ++
                                autogenModulePath ++ ".hs"
                        withFile filePath WriteMode
                                (buildModule
                                        (autogenModuleNameStr
                                                packageDescription
                                        )
                                        (PackageDescription.dataDir
                                                packageDescription
                                        )
                                        (PackageDescription.dataFiles
                                                packageDescription
                                        )
                                )
                )

autogenModuleNameStr :: PackageDescription.PackageDescription -> String
autogenModuleNameStr packageDescription =
        "PathsCache_" ++
        (map
                fixchar
                (PackageName.unPackageName
                        (Package.packageName packageDescription)
                )
        )
        where
                fixchar '-' = '_'
                fixchar c   = c

buildModule :: String -> String -> [String] -> Handle -> IO ()
buildModule moduleName dataDir dataFiles h = do
        hPutStrLn h $ "{-# LANGUAGE OverloadedStrings #-}"
        -- Avoid this warning during compile:
        -- -- Pattern match checker exceeded (2000000) iterations in an equation
        -- -- for ‘getDataFileContent’. (Use -fmax-pmcheck-iterations=n to set
        -- -- the maximun number of iterations to n).
        hPutStrLn h $ "{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}"
        hPutStrLn h $ ""
        hPutStrLn h $ "module " ++ moduleName ++ " (getDataFileContent) where"
        hPutStrLn h $ ""
        -- Warning: The package component's being built has to depend on package
        -- ByteString.
        -- TODO: Create a hook to add ByteString on configure if the cache
        -- module is needed.
        hPutStrLn h $ "import qualified Data.ByteString as BS"
        hPutStrLn h $ ""
        hPutStrLn h $ "getDataFileContent :: String -> BS.ByteString"
        -- For each data file, return its content as a ByteString.
        mapM_
                (\df -> do
                        let (dir,file) = FilePath.splitFileName df
                        let (name,extension) = FilePath.splitExtensions file
                        if (name /= "*")
                                then addFile h df (dataDir ++ "/" ++ df)
                                else do
                                        dirFiles <- Directory.listDirectory
                                                (dataDir ++ "/" ++ dir)
                                        mapM_
                                                (\dirFile -> addFile h
                                                        ((FilePath.dropFileName df) ++ (FilePath.takeFileName dirFile))
                                                        (dataDir ++ "/" ++ dir ++ dirFile)
                                                )
                                                (filter
                                                        (\a -> (FilePath.takeExtensions a) == extension)
                                                        dirFiles
                                                )
                )
                dataFiles
        -- Make the otherwise case an error.
        hPutStrLn h $ "getDataFileContent fp = error $ \"getDataFileContent: \" ++ (show fp)"

addFile :: Handle -> String -> String -> IO ()
addFile h dataFile filePath = do
        bs <- BS.readFile filePath
        hPutStrLn h $
                "getDataFileContent " ++
                "\"" ++ dataFile ++ "\"" ++
                " = " ++ (show bs)
