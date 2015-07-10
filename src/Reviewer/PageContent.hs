module Reviewer.PageContent(pageContentFromText,pageContentToText,PageContent) where

import ClassyPrelude

newtype PageContent = PageContent Text

pageContentFromText :: Text -> PageContent
pageContentFromText = PageContent

pageContentToText :: PageContent -> Text
pageContentToText (PageContent x) = x
