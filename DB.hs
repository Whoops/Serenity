{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module DB where
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Artist
  name String
  deriving (Show)
Album
  name String
  artist ArtistId
  deriving (Show)
Track
  file String
  title String
  artist ArtistId
  album AlbumId
  genre String
  comment String
  track Int
  year Int
  deriving (Show)
|]

main :: IO()
main = runSqlite "db.sqlite" $ do
  runMigration migrateAll