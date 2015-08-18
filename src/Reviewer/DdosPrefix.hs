{-# LANGUAGE TemplateHaskell #-}
module Reviewer.DdosPrefix where

import ClassyPrelude
import Control.Lens(makeLenses)

data DdosPrefix = DdosPrefix {
    _ddosName :: Text
  , _ddosValue :: Text
  }

$(makeLenses ''DdosPrefix)
