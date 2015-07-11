module Reviewer.Url(Url,urlAsText,urlAsString) where

import ClassyPrelude
import Control.Lens(Iso',iso)
import Data.Text.Lens(unpacked)

newtype Url = Url Text deriving(Show,Eq)

urlAsText :: Iso' Url Text
urlAsText = iso (\(Url t) -> t) Url

urlAsString :: Iso' Url String
urlAsString = urlAsText . unpacked
