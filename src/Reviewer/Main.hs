module Main where

import ClassyPrelude hiding(getCurrentTime,elem,writeFile,readFile)
import Reviewer.PageContent
import Reviewer.PageNumber
import Reviewer.Time
import qualified Shelly as Shelly
import Reviewer.EntityType
import Reviewer.RelevantLink
import Control.Concurrent.Async(mapConcurrently)
import Data.Text(splitOn)
import Data.Text.Lazy.Builder(toLazyText)
import Data.Aeson(toJSON,encode,decode)
import Data.Aeson.Types(defaultOptions)
import Data.Aeson.Encode(encodeToTextBuilder)
import Reviewer.Settings
import Reviewer.Entity
import qualified System.Console.Haskeline as HL
import Reviewer.Url
import Reviewer.Database
import Control.Lens((^.),(&),(<>~),view,filtered,(.~),Getting,from)
import Network.Wreq(get,responseBody)
import Data.Text.Lens(packed)
import Data.ByteString.Lazy(writeFile,readFile)

outputStrLn :: MonadIO m => Text -> HL.InputT m ()
outputStrLn s = HL.outputStrLn (unpack s)

makePageUrl :: Settings -> PageNumber -> Url
makePageUrl settings page = ((settings ^. settingsBaseUrl) <> "&page=" <> pack (show (extractPageNumber page))) ^. from urlAsText

retrieveUrl :: MonadIO m => Url -> m PageContent
retrieveUrl u = do
  {-
  let opts = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 10000 } )
  in undefined
  -}
  response <- liftIO (get (u ^. urlAsString))
  return (pageContentFromText (decodeUtf8 (toStrict (response ^. responseBody))))

extractLinks :: PageContent -> [RelevantLink]
extractLinks = undefined

classifyLink :: Database -> RelevantLink -> Maybe Entity
classifyLink db link = find (\entity -> (entity ^. entityText) `isInfixOf` (link ^. rlText)) db

readDatabase :: MonadIO m => Settings -> m Database
readDatabase settings = do
  fileContent <- liftIO (readFile (settings ^. settingsDbFile))
  case decode fileContent of
    Nothing -> error "Invalid data base"
    Just s -> return s

writeDatabase :: MonadIO m => Settings -> Database -> m ()
writeDatabase settings db = do
  liftIO (writeFile (settings ^. settingsDbFile) (encode (toJSON db)))

eqL :: Eq a => Getting a t a -> t -> t -> Bool
eqL l a b = view l a == view l b

updateDatabase :: Database -> Entity -> Database
updateDatabase db entity = db & traverse . filtered (eqL entityText entity) .~ entity

openBrowser :: MonadIO m => Settings -> Url -> m ()
openBrowser settings url =
  let (command:args) = splitOn " " (settings ^. settingsBrowserBin . packed)
  in Shelly.shelly $ Shelly.run_ (Shelly.fromText command) (args <> [url ^. urlAsText])

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

processLink :: (MonadIO m,HL.MonadException m) => Settings -> RelevantLink -> HL.InputT m ()
processLink settings link = do
  db <- readDatabase settings
  currentTime <- getCurrentTime
  case classifyLink db link of
    Nothing -> do
      outputStrLn $ "Entity \"" <> link ^. rlText <> "\" is unknown, opening"
      openBrowser settings (link ^. rlUrl)
      c' <- readCharConditional "(g)ood | (b)ad | (i)ndet: " readEntityState
      case c' of
        Nothing -> return ()
        Just c -> do
          let lonName = longestName (link ^. rlText)
          name <- HL.getInputLine (unpack ("Which name? ["<> lonName <>"] "))
          case name of
            Nothing -> return ()
            Just newName -> do
              writeDatabase settings ((Entity{_entityType = c,_entityText = if null newName then lonName else pack newName,_entityEncounters = [currentTime]}):db)
              outputStrLn $ "Updated database!"
    Just entity ->
      let
        editedEnt = entity & entityEncounters <>~ [currentTime]
      in
        case entity ^. entityType of
          EntityBad -> do
            outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is bad, ignoring"
            writeDatabase settings (updateDatabase db editedEnt)
          EntityGood -> do
            outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is good, opening"
            writeDatabase settings (updateDatabase db editedEnt)
            openBrowser settings (link ^. rlUrl)
          EntityIndet -> do
            outputStrLn $ "Entity \"" <> entity ^. entityText <> "\" is indeterminate, opening"
            openBrowser settings (link ^. rlUrl)
            c' <- readCharConditional "(g)ood | (b)ad | (i)ndet: " readEntityState
            case c' of
              Nothing -> return ()
              Just c -> do
                writeDatabase settings (updateDatabase db (editedEnt & entityType .~ c))
                outputStrLn $ "Updated data base!"

main :: IO ()
main = do
  --ctime <- getCurrentTime
  --putStrLn (toStrict (toLazyText (encodeToTextBuilder (toJSON ([Entity{_entityType = EntityGood,_entityText = "awesome",_entityEncounters = [ctime]}])))))
  let settings = (Settings{_settingsDbFile="/tmp/db.json",_settingsBrowserBin="/usr/bin/google-chrome --incognito"})
  HL.runInputT HL.defaultSettings (processLink settings (RelevantLink{_rlText="anderertext",_rlUrl="http://php-tech.de" ^. from urlAsText}))
  --db <-readDatabase settings
  --putStrLn . toStrict . toLazyText . encodeToTextBuilder $ (toJSON db)
  {-
  settings <- parseSettings
  let
    pages = [1..settings ^. settingsPages]
    pageUrls = (makePageUrl settings . pageNumber) <$> pages
  pageContents <- mapConcurrently retrieveUrl pageUrls
  let
    relevantLinks = concatMap extractLinks pageContents
  HL.runInputT HL.defaultSettings (mapM_ (processLink settings) relevantLinks)
  -}
  
  
