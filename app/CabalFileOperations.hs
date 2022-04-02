{-# LANGUAGE RecordWildCards #-}

module CabalFileOperations where

import Cabal.Package (readPackage)
import Common (FilePath (..), ModuleName (..))
import Control.Monad (when)
import qualified Distribution.Compat.Lens as L
import qualified Distribution.ModuleName as DM
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Types.BuildInfo.Lens (otherModules)
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Executable.Lens (exeBuildInfo)
import Distribution.Types.GenericPackageDescription.Lens
  ( condExecutables,
    condTestSuites,
    packageDescription,
  )
import Distribution.Types.TestSuite.Lens (testBuildInfo)
import Prelude hiding (FilePath)

addExecutableModule :: FilePath -> ModuleName -> IO ()
addExecutableModule (FilePath cabalFilePath) (ModuleName newModuleName) = do
  genericDesc <- readPackage cabalFilePath
  let packageDescription' = L.view packageDescription genericDesc
      condExecutables' = L.view condExecutables genericDesc
  when (null condExecutables') $ error ("No executables have been found in " <> show cabalFilePath)
  let (xs, node) = head condExecutables'
      CondNode {..} = node
      buildInfo = L.view exeBuildInfo condTreeData
      newModules = L.over otherModules (++ [DM.fromString newModuleName]) buildInfo
      newExecutables = [(xs, node {condTreeData = L.set exeBuildInfo newModules condTreeData})]
      newDescription = L.set condExecutables newExecutables genericDesc
  writeGenericPackageDescription cabalFilePath newDescription

addTestSuiteModule :: FilePath -> ModuleName -> IO ()
addTestSuiteModule (FilePath cabalFilePath) (ModuleName newModuleName) = do
  genericDesc <- readPackage cabalFilePath
  let condTestSuites' = L.view condTestSuites genericDesc
  when (null condTestSuites') $ error ("No test-suites have been found in " <> show cabalFilePath)
  let (xs, node) = head condTestSuites'
      CondNode {..} = node
      buildInfo = L.view testBuildInfo condTreeData
      newModules = L.over otherModules (++ [DM.fromString newModuleName]) buildInfo
      newTestSuites = [(xs, node {condTreeData = L.set testBuildInfo newModules condTreeData})]
      newDescription = L.set condTestSuites newTestSuites genericDesc
  writeGenericPackageDescription cabalFilePath newDescription
