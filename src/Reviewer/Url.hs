module Reviewer.Url(Url,extractUrlText,url,extractUrlString) where

import ClassyPrelude

newtype Url = Url Text deriving(Show,Eq)

extractUrlText :: Url -> Text
extractUrlText (Url x) = x

extractUrlString :: Url -> String
extractUrlString (Url x) = unpack x

url :: Text -> Url
url = Url
