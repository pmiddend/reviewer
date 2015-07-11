{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import ClassyPrelude hiding(getCurrentTime,elem,writeFile,readFile,Element,FilePath)
import Data.Maybe(fromJust)
import Text.Taggy.Lens(html,allNamed,allAttributed,Element,contents,attr)
import Reviewer.PageContent
import Reviewer.LinkRange
import Control.Monad(foldM_)
import Reviewer.PageNumber
import Reviewer.Time
import qualified Shelly
import Reviewer.EntityType
import Reviewer.RelevantLink
import Control.Concurrent.Async(mapConcurrently)
import Data.Text(splitOn)
import Reviewer.Settings
import Reviewer.Entity
import qualified System.Console.Haskeline as HL
import Reviewer.Url
import Reviewer.Database
import Control.Lens((^.),(&),(<>~),view,filtered,(.~),Getting,from,only,ix,to,(^..),(^?!),has)
import qualified Network.Wreq as Wreq
import Data.Text.Lens(packed)

outputStrLn :: MonadIO m => Text -> HL.InputT m ()
outputStrLn s = HL.outputStrLn (unpack s)

makePageUrl :: Settings -> PageNumber -> Url
makePageUrl settings page = (pack (settings ^. settingsBaseUrl) <> "/forumdisplay.php?f=" <> pack (settings ^. settingsSubForum) <> "&order=desc&page=" <> pack (show (extractPageNumber page))) ^. from urlAsText

retrieveUrl :: MonadIO m => Url -> m PageContent
retrieveUrl u = do
  {-
  let opts = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 10000 } )
  in undefined
  -}
  putStrLn $ "retrieving " <> pack (show u)
  response <- liftIO (Wreq.get (u ^. urlAsString))
  return $ decodeUtf8 (toStrict (response ^. Wreq.responseBody)) ^. from pageContentAsText

extractLinks :: PageContent -> [RelevantLink]
extractLinks page = page ^.. pageContentAsStrictText . html . allNamed (only "a") . allAttributed (ix "id" . filtered ("thread_title" `isPrefixOf`)) . to relevantLink
  where relevantLink :: Element -> RelevantLink
        relevantLink l = RelevantLink {
            _rlText = l ^. contents
          , _rlUrl = fromJust (l ^?! attr "href") ^. from urlAsText
          }

classifyLink :: Database -> RelevantLink -> Maybe Entity
classifyLink db link = find (\entity -> (entity ^. entityText) `isInfixOf` (link ^. rlText)) db

eqL :: Eq a => Getting a t a -> t -> t -> Bool
eqL l a b = view l a == view l b

updateDatabase :: Database -> Entity -> Database
updateDatabase db entity = db & traverse . filtered (eqL entityText entity) .~ entity

openBrowser :: MonadIO m => Settings -> Url -> m ()
openBrowser settings url =
  let (command:args) = splitOn " " (settings ^. settingsBrowserBin . packed)
  in Shelly.shelly $ Shelly.run_ (Shelly.fromText command) (args <> [pack (settings ^. settingsBaseUrl) <> "/" <> (url ^. urlAsText)])

readCharConditional :: (MonadIO m,HL.MonadException m) => String -> (Char -> Maybe a) -> HL.InputT m (Maybe a)
readCharConditional s f = do
  c' <- HL.getInputChar s
  case c' of
    Nothing -> return Nothing
    Just c ->
      case f c of
        Nothing -> readCharConditional s f
        Just r -> return (Just r)

readEntityState :: Char -> Maybe EntityType
readEntityState 'g' = Just EntityGood
readEntityState 'b' = Just EntityBad
readEntityState 'i' = Just EntityIndet
readEntityState _ = Nothing

-- The implicit contract for splitOn guarantees that it returns at
-- least one element, which we make use of here to construct
-- MinLen (Succ Zero)
splitOnSafe :: Text -> Text -> MinLen (Succ Zero) [Text]
splitOnSafe a b = unsafeToMinLen (splitOn a b)

longestName :: Text -> Text
longestName = maximumBy (compare `on` length) . splitOnSafe " / "

processLink :: forall m.(MonadIO m,HL.MonadException m) => Settings -> [Entity] -> LinkRange -> RelevantLink -> HL.InputT m [Entity]
processLink settings previousEntities linkRange link = do
  db <- readDatabase (settings ^. settingsDbFile)
  currentTime <- getCurrentTime
  outputStrLn (visualizeLinkRange linkRange)
  case classifyLink db link of
    Nothing -> do
      outputStrLn $ "Entity \"" <> link ^. rlText <> "\" is unknown, opening"
      openBrowser settings (link ^. rlUrl)
      c' <- readCharConditional "(g)ood | (b)ad | (i)ndet: " readEntityState
      case c' of
        Nothing -> return previousEntities
        Just c -> do
          let lonName = longestName (link ^. rlText)
          name <- HL.getInputLine (unpack ("Which name? ["<> lonName <>"] "))
          case name of
            Nothing -> return previousEntities
            Just newName -> do
              let newEntity = Entity{_entityType = c,_entityText = if null newName then lonName else pack newName,_entityEncounters = [currentTime]}
              writeDatabase (settings ^. settingsDbFile) (newEntity:db)
              outputStrLn "Updated database!"
              return (newEntity : previousEntities)
    Just entity ->
      if has (traverse . entityText . only (entity ^. entityText)) previousEntities
        then do
          outputStrLn "Entity already encountered, ignoring"
          return previousEntities
        else do
          let
              editedEnt = entity & entityEncounters <>~ [currentTime]
          outputStrLn "Previous encounters:"
          mapM_ (outputStrLn . pack .show) (entity ^. entityEncounters)
          outputStrLn ""
          case entity ^. entityType of
              EntityBad -> do
                outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is bad, ignoring"
                writeDatabase (settings ^. settingsDbFile) (updateDatabase db editedEnt)
                return (entity : previousEntities)
              EntityGood -> do
                outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is good, opening"
                writeDatabase (settings ^. settingsDbFile) (updateDatabase db editedEnt)
                openBrowser settings (link ^. rlUrl)
                return (entity : previousEntities)
              EntityIndet -> do
                outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is indeterminate, opening"
                openBrowser settings (link ^. rlUrl)
                c' <- readCharConditional "(g)ood | (b)ad | (i)ndet: " readEntityState
                case c' of
                    Nothing ->
                      return (entity : previousEntities)
                    Just c -> do
                      writeDatabase (settings ^. settingsDbFile) (updateDatabase db (editedEnt & entityType .~ c))
                      outputStrLn "Updated data base!"
                      return (entity : previousEntities)

main :: IO ()
main = do
  --videosPage <- TIO.readFile "/tmp/videos.html"
  --print (extractLinks (videosPage ^. from pageContentAsText))
  --ctime <- getCurrentTime
  --putStrLn (toStrict (toLazyText (encodeToTextBuilder (toJSON ([Entity{_entityType = EntityGood,_entityText = "awesome",_entityEncounters = [ctime]}])))))
  --let settings = (Settings{_settingsDbFile="/tmp/db.json",_settingsBrowserBin="/usr/bin/google-chrome --incognito"})
  --HL.runInputT HL.defaultSettings (processLink settings (RelevantLink{_rlText="anderertext",_rlUrl="http://php-tech.de" ^. from urlAsText}))
  --db <-readDatabase settings
  --putStrLn . toStrict . toLazyText . encodeToTextBuilder $ (toJSON db)
  settings <- parseSettings
  let
    pages = [1..settings ^. settingsPages]
    pageUrls = (makePageUrl settings . pageNumber) <$> pages
  pageContents <- mapConcurrently retrieveUrl pageUrls
  let
    relevantLinks = concatMap extractLinks pageContents
  HL.runInputT HL.defaultSettings $
    foldM_ (\previousEntities (i,l) -> processLink settings previousEntities (LinkRange i (length relevantLinks)) l) [] (zip [1..] relevantLinks)
  
  
