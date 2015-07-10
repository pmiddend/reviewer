{-# LANGUAGE TemplateHaskell #-}
module Reviewer.Settings(Settings(..),settingsPages,settingsDataDir,settingsBrowserBin,settingsBaseUrl,parseSettings) where

import ClassyPrelude hiding(FilePath,(<>))
import System.FilePath
import Options.Applicative(strOption,long,help,option,auto,(<>),Parser,execParser,helper,fullDesc,progDesc,header,info)
import Control.Lens(makeLenses)

data Settings = Settings {
    _settingsPages :: Int
  , _settingsDataDir :: FilePath
  , _settingsBrowserBin :: FilePath
  , _settingsBaseUrl :: Text
  } deriving(Show,Eq,Read)

$(makeLenses ''Settings)

parseSettings' :: Parser Settings
parseSettings' = Settings
                <$> option auto (long "pages" <> help "How many pages to load")
                <*> strOption (long "data-dir" <> help "Directory for loading/storing data")
                <*> strOption (long "browser-bin" <> help "Location of the browser")
                <*> option auto (long "base-url" <> help "Base URL to load")

parseSettings :: MonadIO m => m Settings
parseSettings = liftIO (execParser opts)
  where
    opts = info (helper <*> parseSettings')
      (fullDesc <> progDesc "Load forum pages, review their content" <> header "Load forum pages, review their content")
