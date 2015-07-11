module Main where

import ClassyPrelude
import Reviewer.Entity
import Reviewer.EntityType
import Reviewer.Database

makeEntity :: EntityType -> Text -> Entity
makeEntity goodOrBad text = Entity {
    _entityText = text
  , _entityType = goodOrBad
  , _entityEncounters = []
  }

toEntities :: (MonadIO m, Functor m) => FilePath -> EntityType -> m [Entity]
toEntities f goodOrBad = do
  ls <- lines <$> readFile f
  return (map (makeEntity goodOrBad) ls)

main :: IO ()
main = do
  goodEntities <- toEntities "good.txt" EntityGood
  badEntities <- toEntities "bad.txt" EntityBad
  writeDatabase "/tmp/output.db" (goodEntities <> badEntities)
