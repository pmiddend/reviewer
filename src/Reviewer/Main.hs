module Main where

import ClassyPrelude hiding(getCurrentTime,elem)
import Reviewer.PageContent
import Reviewer.PageNumber
import Reviewer.Time
import Reviewer.EntityType
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Reviewer.RelevantLink
import Data.List(elem)
import Control.Concurrent.Async(mapConcurrently)
import Reviewer.Settings
import Reviewer.Entity
import qualified System.Console.Haskeline as HL
import Reviewer.Url
import Reviewer.Database
import Control.Lens((^.),(&),(<>~),view,filtered,(.~),Getting)
import Network.Wreq(get,responseBody)

outputStrLn :: MonadIO m => Text -> HL.InputT m ()
outputStrLn s = HL.outputStrLn (unpack s)

makePageUrl :: Settings -> PageNumber -> Url
makePageUrl settings page = url ((settings ^. settingsBaseUrl) <> "&page=" <> pack (show (extractPageNumber page)))


retrieveUrl :: MonadIO m => Url -> m PageContent
retrieveUrl u = do
  {-
  let opts = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 10000 } )
  in undefined
  -}
  response <- liftIO (get (extractUrlString u))
  return (pageContentFromText (decodeUtf8 (toStrict (response ^. responseBody))))

extractLinks :: PageContent -> [RelevantLink]
extractLinks = undefined

classifyLink :: Database -> RelevantLink -> Maybe Entity
classifyLink = undefined

readDatabase :: MonadIO m => Settings -> m Database
readDatabase = undefined

writeDatabase :: MonadIO m => Settings -> Database -> m ()
writeDatabase = undefined

eqL :: Eq a => Getting a t a -> t -> t -> Bool
eqL l a b = view l a == view l b

updateDatabase :: Database -> Entity -> Database
updateDatabase db entity = db & traverse . filtered (eqL entityText entity) .~ entity

openBrowser :: MonadIO m => Settings -> Url -> m ()
openBrowser = undefined

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

longestName :: Text -> Text
longestName = undefined

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
          --writeDatabase settings (updateDatabase db (editedEnt & entityType .~ c))
          --outputStrLn $ "Updated data base!"
          let lonName = longestName (link ^. rlText)
          name <- HL.getInputLine (unpack ("Which name? ["<> lonName <>"] "))
          case name of
            Nothing -> return ()
            Just newName -> do
              writeDatabase settings ((Entity{_entityType = c,_entityText = pack newName,_entityEncounters = [currentTime]}):db)
              outputStrLn $ "Updated data base!"
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
  settings <- parseSettings
  let
    pages = [1..settings ^. settingsPages]
    pageUrls = (makePageUrl settings . pageNumber) <$> pages
  pageContents <- mapConcurrently retrieveUrl pageUrls
  let
    relevantLinks = concatMap extractLinks pageContents
  HL.runInputT HL.defaultSettings (mapM_ (processLink settings) relevantLinks)
  
  
