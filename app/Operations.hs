{-# LANGUAGE OverloadedStrings #-}

module Operations (addExeModule, addTestModule) where

import CabalFileOperations (addExecutableModule, addTestSuiteModule)
import Common (FilePath (..), ModuleName (..))
import Control.Monad (when)
import qualified Data.Text as T
import Distribution.ModuleName (fromString, toFilePath)
import qualified Shelly as Sh
import System.FilePath (addExtension, dropFileName, (</>))
import Prelude hiding (FilePath)

mkModule :: ModuleName -> T.Text -> Sh.Sh ()
mkModule (ModuleName modulePath) moduleHeader = do
  let dir = dropFileName modulePath
  when (dir /= "./") $ Sh.bash_ "mkdir -p" [T.pack dir]
  Sh.writefile modulePath moduleHeader

mkHsModule :: FilePath -> ModuleName -> IO ()
mkHsModule (FilePath baseDir) mn@(ModuleName moduleName) = Sh.shelly $ do
  let moduleHeader :: T.Text
      moduleHeader = "module " <> T.pack moduleName <> " where\n"
      modulePath = toFilePath $ fromString moduleName
      newModulePath = addExtension (baseDir </> modulePath) "hs"
  isExists <- Sh.test_f newModulePath
  if isExists then error ("Module " <> show moduleName <> " already exists") else mkModule (ModuleName newModulePath) moduleHeader

addExeModule :: FilePath -> FilePath -> ModuleName -> IO ()
addExeModule cabalFilePath baseDir moduleName = do
  mkHsModule baseDir moduleName
  addExecutableModule cabalFilePath moduleName

addTestModule :: FilePath -> FilePath -> ModuleName -> IO ()
addTestModule cabalFilePath baseDir moduleName = do
  mkHsModule baseDir moduleName
  addTestSuiteModule cabalFilePath moduleName
