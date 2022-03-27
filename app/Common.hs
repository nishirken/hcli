module Common where

newtype ModuleName = ModuleName String deriving (Eq, Show)

newtype FilePath = FilePath String deriving (Eq, Show)
