{-# LANGUAGE TemplateHaskell #-}
module Reviewer.EntityType where

import Control.Lens(makePrisms)
import ClassyPrelude
import Data.Aeson.TH(deriveJSON)
import Data.Aeson.Types(defaultOptions)

data EntityType = EntityGood
                | EntityBad
                | EntityIndet
                deriving(Show,Eq,Bounded,Enum)

$(makePrisms ''EntityType)
$(deriveJSON defaultOptions ''EntityType)
