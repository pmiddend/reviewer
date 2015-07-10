module Reviewer.PageNumber(PageNumber,extractPageNumber,pageNumber) where

import ClassyPrelude

newtype PageNumber = PageNumber Int deriving(Show)

pageNumber :: Int -> PageNumber
pageNumber = PageNumber

extractPageNumber :: PageNumber -> Int
extractPageNumber (PageNumber x) = x
