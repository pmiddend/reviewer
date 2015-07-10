{-# LANGUAGE TemplateHaskell #-}
module Reviewer.EntityType where

import Control.Lens(makePrisms)
import ClassyPrelude

data EntityType = EntityGood
                | EntityBad
                | EntityIndet
                deriving(Show,Eq,Bounded,Enum)

$(makePrisms ''EntityType)
