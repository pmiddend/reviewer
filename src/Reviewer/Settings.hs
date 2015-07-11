{-# LANGUAGE TemplateHaskell #-}
module Reviewer.Settings(Settings(..),settingsPages,settingsDbFile,settingsBrowserBin,settingsBaseUrl,parseSettings) where

import ClassyPrelude hiding(FilePath,(<>))
import System.FilePath
import Options.Applicative(strOption,long,help,option,auto,(<>),Parser,execParser,helper,fullDesc,progDesc,header,info)
import Control.Lens(makeLenses)

data Settings = Settings {
    _settingsPages :: Int
  , _settingsDbFile :: FilePath
  , _settingsBrowserBin :: FilePath
  , _settingsBaseUrl :: String
  } deriving(Show,Eq,Read)

$(makeLenses ''Settings)

parseSettings' :: Parser Settings
parseSettings' = Settings
                <$> option auto (long "pages" <> help "How many pages to load")
                <*> strOption (long "db-file" <> help "Name of the data base")
                <*> strOption (long "browser-bin" <> help "Location of the browser")
                {-<*> option auto (long "base-url" <> help "Base URL to load")-}
                <*> strOption (long "base-url" <> help "Base URL to load")

parseSettings :: MonadIO m => m Settings
parseSettings = liftIO (execParser opts)
  where
    opts = info (helper <*> parseSettings')
      (fullDesc <> progDesc "Load forum pages, review their content" <> header "Load forum pages, review their content")
