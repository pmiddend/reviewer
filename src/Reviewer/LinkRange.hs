{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module Reviewer.LinkRange where

import ClassyPrelude
import Control.Lens(makeLenses,(^.),Getter,to)

data LinkRange = LinkRange {
    _linkThis :: Int
  , _linksTotal :: Int
  }

$(makeLenses ''LinkRange)

linkPercentage :: Int -> Getter LinkRange Int
linkPercentage m = to (\lr -> lr ^. linkThis * m `div` lr ^. linksTotal)                
 
visualizeLinkRange :: LinkRange -> Text
visualizeLinkRange r =
  let m = 80
      pt = r ^. linkPercentage m
  in "[" <> (replicate pt '=') <> replicate (m - pt) ' ' <> "] " <> pack (show (r ^. linkThis)) <> "/" <> pack (show (r ^. linksTotal))
