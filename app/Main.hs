{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli (moduleNameParser)
import Common (FilePath (..), ModuleName (..))
import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Operations (addExeModule, addTestModule)
import qualified Shelly as Sh
import qualified Text.Regex as TR
import Prelude hiding (FilePath)

getCabalFile :: IO FilePath
getCabalFile = do
  xs <- Sh.shelly $ Sh.bash "ls *.cabal" []
  let cabalFileName = if T.null xs then error "No cabal file!" else T.init xs
  pure $ FilePath $ T.unpack cabalFileName

baseDir :: FilePath
baseDir = FilePath "app"

baseTestDir :: FilePath
baseTestDir = FilePath "tests"

main :: IO ()
main = do
  cabalFile <- getCabalFile
  Cli.defaultMain $ do
    let defaultModuleName = "DefaultModuleName"
    moduleName <- Cli.flagParam (Cli.FlagShort 'm') (Cli.FlagRequired $ Right . ModuleName)

    Cli.command "add-exe" $ do
      Cli.action $ \toParam -> do
        addExeModule cabalFile baseDir $ fromMaybe (ModuleName defaultModuleName) $ toParam moduleName
    Cli.command "add-test" $ do
      Cli.action $ \toParam -> do
        addTestModule cabalFile baseTestDir $ fromMaybe (ModuleName defaultModuleName) $ toParam moduleName

