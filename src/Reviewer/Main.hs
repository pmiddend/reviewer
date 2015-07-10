module Main where

import ClassyPrelude hiding(getCurrentTime)
import Reviewer.Settings
import Reviewer.PageContent
import Reviewer.PageNumber
import Reviewer.Time
import Reviewer.EntityType
import Reviewer.RelevantLink
import Control.Concurrent.Async(mapConcurrently)
import Reviewer.Settings
import Reviewer.Entity
import qualified System.Console.Haskeline as HL
import Reviewer.Url
import Reviewer.Database
import Control.Lens(Lens',(^.),(&),(<>~))

outputStrLn :: MonadIO m => Text -> HL.InputT m ()
outputStrLn s = HL.outputStrLn (unpack s)

makePageUrl :: Settings -> PageNumber -> Url
makePageUrl = undefined

retrieveUrl :: MonadIO m => Url -> m PageContent
retrieveUrl = undefined

extractLinks :: PageContent -> [RelevantLink]
extractLinks = undefined

classifyLink :: Database -> RelevantLink -> Maybe Entity
classifyLink = undefined

readDatabase :: MonadIO m => Settings -> m Database
readDatabase = undefined

writeDatabase :: MonadIO m => Settings -> Database -> m ()
writeDatabase = undefined

openBrowser :: MonadIO m => Settings -> Url -> m ()
openBrowser = undefined

processLink :: MonadIO m => Settings -> RelevantLink -> HL.InputT m ()
processLink settings link = do
  db <- readDatabase settings
  currentTime <- getCurrentTime
  case classifyLink db link of
    Nothing -> undefined
    Just entity ->
      let
        editedEnt = entity & entityEncounters <>~ [currentTime]
      in
        case entity ^. entityType of
          EntityBad -> do
            outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is bad, ignoring"
            
          
        

main :: IO ()
main = do
  settings <- parseSettings
  let
    pages = [1..settings ^. settingsPages]
    pageUrls = (makePageUrl settings . PageNumber) <$> pages
  pageContents <- mapConcurrently retrieveUrl pageUrls
  let
    relevantLinks = concatMap extractLinks pageContents
  return ()
  --runInputT defaultSettings $ do
  
  
