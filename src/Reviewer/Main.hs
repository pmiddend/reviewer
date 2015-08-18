{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Main where

import           ClassyPrelude            hiding (Element, FilePath, elem,
                                           getCurrentTime, readFile, writeFile)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens             (Getting, filtered, from, has, ix,
                                           only, to, view, (&), (.~), (<>~),
                                           (^.), (^..), (^?!))
import           Control.Monad            (foldM_)
import           Data.Maybe               (fromJust)
import           Data.Text                (splitOn,breakOn)
import           Data.Text.Lens           (packed)
import qualified Network.Wreq             as Wreq
import qualified Data.CaseInsensitive as CI
import           Reviewer.Database
import           Reviewer.Entity
import           Reviewer.EntityType
import           Reviewer.LinkRange
import           Reviewer.PageContent
import           Reviewer.PageNumber
import           Reviewer.RelevantLink
import           Reviewer.Settings
import           Reviewer.Time
import           Reviewer.DdosPrefix
import           Reviewer.Url
import qualified Shelly
import qualified System.Console.Haskeline as HL
import           Text.Taggy.Lens          (Element, allAttributed, allNamed,
                                           attr, contents, html)

outputStrLn :: MonadIO m => Text -> HL.InputT m ()
outputStrLn s = HL.outputStrLn (unpack s)

makePageUrl :: Settings -> PageNumber -> Url
makePageUrl settings page = (pack (settings ^. settingsBaseUrl) <> "/forumdisplay.php?f=" <> pack (settings ^. settingsSubForum) <> "&order=desc&page=" <> pack (show (extractPageNumber page))) ^. from urlAsText

retrieveUrl :: MonadIO m => DdosPrefix -> Url -> m PageContent
retrieveUrl t u = do
  {-
  let opts = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 10000 } )
  in undefined
  -}
  putStrLn $ "retrieving " <> pack (show u)
  response <- liftIO (Wreq.getWith (Wreq.defaults & Wreq.headers <>~ [(CI.mk "Cookie",encodeUtf8 (t ^. ddosName <> "=" <> t ^. ddosValue))]) (u ^. urlAsString))
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
                outputStrLn $ "Original link: " <> link ^. rlText
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

extractBetween :: Text -> Text -> Text -> Text
extractBetween prefix suffix text =
  let
    (_,prefixAndAfter) = breakOn prefix text
    (match,_) = breakOn suffix prefixAndAfter
  in
   match

extractDdosPrefix :: Settings -> IO (Maybe DdosPrefix)
extractDdosPrefix settings = do
  getResult <- Wreq.get (settings ^. settingsBaseUrl)
  let
    resultText = getResult ^. Wreq.responseBody . to (decodeUtf8 . toStrict)
    between = extractBetween (settings ^. settingsDdosPrefix . packed <> "=") ";" resultText
    (before,equalsAndAfter) = breakOn "=" between
  return (if null between then Nothing else Just (DdosPrefix before (drop 1 equalsAndAfter)))

maybeFlipped :: Maybe a -> b -> (a -> b) -> b
maybeFlipped m d f = maybe d f m

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
  putStrLn "Extracting prefix..."
  ddosPrefix' <- extractDdosPrefix settings
  maybeFlipped ddosPrefix' (putStrLn "Prefix not found, please check") $ \ddosPrefix -> do
    let
      pages = [1..settings ^. settingsPages]
      pageUrls = (makePageUrl settings . pageNumber) <$> pages
    pageContents <- mapConcurrently (retrieveUrl ddosPrefix) pageUrls
    let
      relevantLinks = concatMap extractLinks pageContents
    HL.runInputT HL.defaultSettings $
        foldM_ (\previousEntities (i,l) -> processLink settings previousEntities (LinkRange i (length relevantLinks)) l) [] (zip [1..] relevantLinks)


