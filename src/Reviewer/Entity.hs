{-# LANGUAGE TemplateHaskell #-}
module Reviewer.Entity where

import Reviewer.EntityType
import Control.Lens(makeLenses)
import ClassyPrelude

data Entity = Entity {
    _entityText :: Text
  , _entityType :: EntityType
  , _entityEncounters :: [UTCTime]
  } deriving(Show,Eq)

$(makeLenses ''Entity)
