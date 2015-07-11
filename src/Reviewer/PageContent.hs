module Reviewer.PageContent(PageContent,pageContentAsText,pageContentAsStrictText) where

import ClassyPrelude
import Control.Lens(Iso',iso)
import qualified Data.Text.Lazy as TL

newtype PageContent = PageContent Text deriving(Show,Eq)

pageContentAsText :: Iso' PageContent Text
pageContentAsText = iso (\(PageContent c) -> c) PageContent

pageContentAsStrictText :: Iso' PageContent TL.Text
pageContentAsStrictText = iso (\(PageContent c) -> TL.fromStrict c) (PageContent . TL.toStrict)
