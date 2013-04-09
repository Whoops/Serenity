{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, OverloadedStrings #-}
module DB where
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Maybe(fromJust)
import Data.Aeson
import Data.SafeCopy
import Data.Acid
import Data.Typeable
import Data.IxSet
import qualified Data.Set as Set

newtype ArtistId = ArtistId { unArtistId :: Integer }
                   deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype ArtistName = ArtistName { unArtistName :: String }
                     deriving(Eq, Ord, Show, SafeCopy, Typeable)
newtype AlbumId = AlbumId { unAlbumId :: Integer }                            
                  deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype AlbumName = AlbumName {unAlbumName :: String }
                    deriving(Eq, Ord, Show, SafeCopy, Typeable)
newtype TrackId = TrackId { unTrackId :: Integer }
                  deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype TrackTitle = TrackTitle { unTrackTitle :: String }
                     deriving(Eq, Ord, Show, SafeCopy, Typeable)

data Database = Database { nextArtist :: ArtistId,
                           nextAlbum :: AlbumId,
                           nextTrack :: TrackId,
                           artists :: IxSet Artist, 
                           albums :: IxSet Album, 
                           tracks :: IxSet Track }
                deriving (Show, Typeable)
data Artist = Artist { artistId :: ArtistId,
                       artistName :: String, 
                       artistAlbums :: Set.Set AlbumId,
                       artistTracks :: Set.Set TrackId }
            deriving (Eq, Ord, Show, Typeable)
data Album = Album { albumId :: AlbumId,
                     albumName :: String, 
                     albumTracks :: Set.Set TrackId }
           deriving (Eq, Ord, Show, Typeable)
data Track = Track { trackId :: TrackId,
                     file :: FilePath,
                     artist :: ArtistId,
                     album :: AlbumId,
                     title :: String,
                     genre :: String,
                     comment :: String,
                     track :: Integer,
                     year :: Integer }
           deriving (Eq, Ord, Show, Typeable)         
                    
instance ToJSON ArtistId where
  toJSON (ArtistId id) = Number $ fromIntegral id
instance ToJSON AlbumId where
  toJSON (AlbumId id) = Number $ fromIntegral id
instance ToJSON TrackId where
  toJSON (TrackId id) = Number $ fromIntegral id
instance ToJSON Artist where
  toJSON (Artist id name albums tracks) = object ["id" .= id, "name" .= name, "albums" .= albums, "tracks" .= tracks]
instance ToJSON Album where
  toJSON (Album id name tracks) = object ["id" .= id, "name" .= name, "tracks" .= tracks]
instance ToJSON Track where
  toJSON (Track id _ artist album title genre comment track year) = object ["id" .= id,
                                                                            "artist" .= artist,
                                                                            "album" .= album,
                                                                            "title" .= title,
                                                                            "genre" .= genre,
                                                                            "comment" .= comment,
                                                                            "track" .= track,
                                                                            "year" .= year]

$(deriveSafeCopy 0 'base ''Album)
$(deriveSafeCopy 0 'base ''Track)
$(deriveSafeCopy 0 'base ''Artist)
$(deriveSafeCopy 0 'base ''Database)

instance Indexable Artist where 
  empty = ixSet [ ixFun $ \art -> [artistId art],
                  ixFun $ \art -> [ArtistName $ artistName art] ]
instance Indexable Album where
  empty = ixSet [ ixFun $ \alb -> [albumId alb],
                  ixFun $ \alb -> [AlbumName $ albumName alb] ]
instance Indexable Track where
  empty = ixSet [ ixFun $ \track -> [trackId track],
                  ixFun $ \track -> [artist track],
                  ixFun $ \track -> [album track], 
                  ixFun $ \track -> [TrackTitle $ title track] ]
  
addArtist :: String ->  Update Database Artist
addArtist name = do
  db@Database{..} <- get
  let art = Artist nextArtist name Set.empty Set.empty
  put $ db { nextArtist = succ nextArtist,
             artists = insert art artists }
  return art

getOrCreateArtist :: String -> Update Database Artist
getOrCreateArtist name = do
  db@Database{..} <- get
  let art = artists @= (ArtistName name)
  if Data.IxSet.null art then
    addArtist name
  else
    return $ fromJust $ getOne art
  
addAlbum :: String ->  Update Database Album
addAlbum name = do
  db@Database{..} <- get
  let alb = Album nextAlbum name Set.empty
  put $ db { nextAlbum = succ nextAlbum,
             albums = insert alb albums }
  return alb

getOrCreateAlbum :: String -> Update Database Album
getOrCreateAlbum name = do
  db@Database{..} <- get
  let alb = albums @= (AlbumName name)
  if Data.IxSet.null alb then
    addAlbum name
  else
    return $ fromJust $ getOne alb
         
addTrack :: FilePath -> String -> String -> String -> String -> String -> Integer -> Integer -> Update Database Track
addTrack file artist album title genre comment track year = do
  alb <- getOrCreateAlbum album 
  art <- getOrCreateArtist artist
  d@Database{..} <- get
  let tr = Track nextTrack file (artistId art) (albumId alb) title genre comment track year
  let updatedArtist = art { artistAlbums = Set.insert (albumId alb) (artistAlbums art),
                            artistTracks = Set.insert (trackId tr) (artistTracks art) }
  let updatedAlbum = alb { albumTracks = Set.insert (trackId tr) (albumTracks alb) }
  put $ d { nextTrack = succ nextTrack,
             tracks = insert tr tracks, 
             artists = updateIx (artistId art) updatedArtist artists,
             albums = updateIx (albumId alb) updatedAlbum albums}
  return tr

getArtists :: Query Database [Artist]
getArtists = do
  d@Database{..} <- ask
  return $ toList artists

getAlbums :: Query Database [Album]
getAlbums = do
  d@Database{..} <- ask
  return $ toList albums

getTracks :: Query Database [Track]
getTracks = do
  d@Database{..} <- ask
  return $ toList tracks


$(makeAcidic ''Database ['addArtist,
                         'getOrCreateArtist,
                         'addAlbum,
                         'getOrCreateAlbum, 
                         'addTrack,
                         'getArtists,
                         'getAlbums,
                         'getTracks])

main :: IO()
main = do
       database <- openDatabase
       return ()
       
openDatabase = openLocalStateFrom "db/" (Database (ArtistId 1) (AlbumId 1) (TrackId 1) empty empty empty)