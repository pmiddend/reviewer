module Reviewer.Time where

import qualified ClassyPrelude as P

getCurrentTime :: P.MonadIO m => m P.UTCTime
getCurrentTime = P.liftIO P.getCurrentTime
