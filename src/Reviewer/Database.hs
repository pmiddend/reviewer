module Reviewer.Database where

import ClassyPrelude hiding(writeFile,FilePath,readFile)
import Data.Aeson(toJSON,encode,decode)
import Data.ByteString.Lazy(writeFile,readFile)
import Reviewer.Entity
import System.Directory(doesFileExist)
import System.FilePath

type Database = [Entity]

writeDatabase :: MonadIO m => FilePath -> Database -> m ()
writeDatabase file db = do
  liftIO (writeFile file (encode (toJSON db)))

readDatabase :: MonadIO m => FilePath -> m Database
readDatabase file = do
  exists <- liftIO (doesFileExist file)
  if not exists
    then return []
    else do 
      fileContent <- liftIO (readFile file)
      case decode fileContent of
        Nothing -> error "Invalid data base"
        Just s -> return s
