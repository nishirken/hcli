{-# LANGUAGE OverloadedStrings #-}

module Cli where

import Common (ModuleName (..))
import Console.Options (ValueParser)
import qualified Data.Text as T
import Debug.Trace
import qualified Text.Regex as TR

moduleNameParser :: ValueParser ModuleName
moduleNameParser rawValue = response $ T.intercalate "." <$> traversed
  where
    traversed :: Maybe [T.Text]
    traversed = traverse check $ (T.splitOn "." . T.pack) $ traceShow rawValue rawValue
    check :: T.Text -> Maybe T.Text
    check xs = do
      res <- TR.matchRegex (TR.mkRegex "^[a-zA-Z]+$") $ T.unpack xs
      pure $ traceShow xs xs
    response :: Maybe T.Text -> Either String ModuleName
    response (Just moduleName) = Right $ ModuleName $ T.unpack moduleName
    response Nothing = Left "ModuleName doesn't follow the pattern Main or Main.Options"
