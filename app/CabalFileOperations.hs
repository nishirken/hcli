{-# LANGUAGE RecordWildCards #-}

module CabalFileOperations where

import Cabal.Package (readPackage)
import Common (FilePath (..), ModuleName (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Prelude hiding (FilePath)
import Control.Monad (when)
import Distribution.PackageDescription (BuildInfo(otherModules))
import qualified Distribution.ModuleName as DM

addExecutableModule :: FilePath -> ModuleName -> IO ()
addExecutableModule (FilePath cabalFilePath) (ModuleName newModuleName) = do
  genericDesc@GenericPackageDescription {..} <- readPackage cabalFilePath
  when (null condExecutables) $ error ("No executables have been found in " <> show cabalFilePath)
  let
    (xs, node) = head condExecutables
    CondNode {..} = node
    Executable {..} = condTreeData
    BuildInfo {..} = buildInfo
    newModules = otherModules ++ [DM.fromString newModuleName]
    newExecutables = [(xs, node {condTreeData = condTreeData {buildInfo = buildInfo {otherModules = newModules}}})]
    newDescription = genericDesc {condExecutables = newExecutables}
  writeGenericPackageDescription cabalFilePath newDescription

addTestSuiteModule :: FilePath -> ModuleName -> IO ()
addTestSuiteModule (FilePath cabalFilePath) (ModuleName newModuleName) = do
  genericDesc@GenericPackageDescription {..} <- readPackage cabalFilePath
  when (null condTestSuites) $ error ("No test-suites have been found in " <> show cabalFilePath)
  let
    (xs, node) = head condTestSuites
    CondNode {..} = node
    TestSuite {..} = condTreeData
    BuildInfo {..} = testBuildInfo
    newModules = otherModules ++ [DM.fromString newModuleName]
    newTestSuites = [(xs, node {condTreeData = condTreeData {testBuildInfo = testBuildInfo {otherModules = newModules}}})]
    newDescription = genericDesc {condTestSuites = newTestSuites}
  writeGenericPackageDescription cabalFilePath newDescription

