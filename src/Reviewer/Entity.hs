{-# LANGUAGE TemplateHaskell #-}
module Reviewer.Entity where

import Reviewer.EntityType
import Control.Lens(makeLenses)
import ClassyPrelude
import Data.Aeson.TH(deriveJSON)
import Data.Aeson.Types(defaultOptions)

data Entity = Entity {
    _entityText :: Text
  , _entityType :: EntityType
  , _entityEncounters :: [UTCTime]
  } deriving(Show,Eq)

$(makeLenses ''Entity)
$(deriveJSON defaultOptions ''Entity)
