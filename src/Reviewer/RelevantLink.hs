{-# LANGUAGE TemplateHaskell #-}
module Reviewer.RelevantLink where

import ClassyPrelude
import Control.Lens(makeLenses)
import Reviewer.Url

data RelevantLink = RelevantLink {
    _rlText :: Text
  , _rlUrl :: Url
  } deriving(Show,Eq)

$(makeLenses ''RelevantLink)
