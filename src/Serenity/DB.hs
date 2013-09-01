{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoAndIfThenElse, RecordWildCards, OverloadedStrings #-}
module Serenity.DB where
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import System.FilePath
import Data.Maybe(fromJust, isJust)
import Data.Aeson
import Data.SafeCopy
import Data.Acid
import Data.Typeable
import Data.IxSet
import Data.Text
import qualified Data.Set as Set

newtype ArtistId = ArtistId { unArtistId :: Integer }
                   deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype ArtistName = ArtistName { unArtistName :: Text }
                     deriving(Eq, Ord, Show, SafeCopy, Typeable)
newtype AlbumId = AlbumId { unAlbumId :: Integer }                            
                  deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype AlbumName = AlbumName {unAlbumName :: Text }
                    deriving(Eq, Ord, Show, SafeCopy, Typeable)
newtype TrackId = TrackId { unTrackId :: Integer }
                  deriving(Eq, Ord, Enum, Show, SafeCopy, Typeable)
newtype TrackTitle = TrackTitle { unTrackTitle :: Text }
                     deriving(Eq, Ord, Show, SafeCopy, Typeable)
newtype TrackFile = TrackFile { unTrackFile :: FilePath }
                    deriving(Eq, Ord, Show, SafeCopy, Typeable)

data Database = Database { nextArtist :: ArtistId,
                           nextAlbum :: AlbumId,
                           nextTrack :: TrackId,
                           artists :: IxSet Artist, 
                           albums :: IxSet Album, 
                           tracks :: IxSet Track }
                deriving (Show, Typeable)
data Artist = Artist { artistId :: ArtistId,
                       artistName :: Text, 
                       artistAlbums :: Set.Set AlbumId,
                       artistTracks :: Set.Set TrackId }
            deriving (Eq, Ord, Show, Typeable)
data Album = Album { albumId :: AlbumId,
                     albumName :: Text, 
                     albumTracks :: Set.Set TrackId }
           deriving (Eq, Ord, Show, Typeable)
data Track = Track { trackId :: TrackId,
                     file :: FilePath,
                     artist :: ArtistId,
                     album :: AlbumId,
                     title :: Text,
                     genre :: Text,
                     comment :: Text,
                     track :: Int,
                     year :: Int }
           deriving (Eq, Ord, Show, Typeable)         
                    
instance ToJSON ArtistId where
  toJSON (ArtistId id) = Number $ fromIntegral id
instance ToJSON AlbumId where
  toJSON (AlbumId id) = Number $ fromIntegral id
instance ToJSON TrackId where
  toJSON (TrackId id) = Number $ fromIntegral id
instance ToJSON Artist where
  toJSON (Artist id name albums tracks) = object ["id" .= id, "name" .= name]
instance ToJSON Album where
  toJSON (Album id name tracks) = object ["id" .= id, "name" .= name]
instance ToJSON Track where
  toJSON (Track id file artist album title genre comment track year) = object ["id" .= id,
                                                                            "artist" .= artist,
                                                                            "album" .= album,
                                                                            "title" .= title,
                                                                            "genre" .= genre,
                                                                            "comment" .= comment,
                                                                            "track" .= track,
                                                                            "year" .= year, 
                                                                            "file" .= file]

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
                  ixFun $ \track -> [TrackFile $ file track],
                  ixFun $ \track -> [artist track],
                  ixFun $ \track -> [album track], 
                  ixFun $ \track -> [TrackTitle $ title track] ]
  
addArtist :: Text ->  Update Database Artist
addArtist name = do
  db@Database{..} <- get
  let art = Artist nextArtist name Set.empty Set.empty
  put $ db { nextArtist = succ nextArtist,
             artists = insert art artists }
  return art

getOrCreateArtist :: Text -> Update Database Artist
getOrCreateArtist name = do
  db@Database{..} <- get
  let art = artists @= ArtistName name
  if Data.IxSet.null art
    then addArtist name
    else return $ fromJust $ getOne art
  
addAlbum :: Text ->  Update Database Album
addAlbum name = do
  db@Database{..} <- get
  let alb = Album nextAlbum name Set.empty
  put $ db { nextAlbum = succ nextAlbum,
             albums = insert alb albums }
  return alb

getOrCreateAlbum :: Text -> Update Database Album
getOrCreateAlbum name = do
  db@Database{..} <- get
  let alb = albums @= AlbumName name
  if Data.IxSet.null alb then
    addAlbum name
  else
    return $ fromJust $ getOne alb
         
addTrack :: FilePath -> Text -> Text -> Text -> Text -> Text -> Int -> Int -> Update Database Track
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

getArtistAlbums :: ArtistId -> Query Database [Album]
getArtistAlbums artistId = do
  d@Database{..} <- ask
  let arts = artists @= artistId
  if Data.IxSet.null arts then
    return []
  else do
    let artist = fromJust $ getOne arts
    let albs = albums @+ Set.toList (artistAlbums artist) 
    return $ toList albs

getArtistTracks :: ArtistId -> Query Database [Track]
getArtistTracks artistId = do
  d@Database{..} <- ask
  let arts = artists @= artistId
  if Data.IxSet.null arts then
    return []
  else do
    let artist = fromJust $ getOne arts
    let trs = tracks @+ Set.toList (artistTracks artist)
    return $ toList trs

getAlbums :: Query Database [Album]
getAlbums = do
  d@Database{..} <- ask
  return $ toList albums
  
getAlbumTracks :: AlbumId -> Query Database [Track]
getAlbumTracks albumId = do
  d@Database{..} <- ask
  let albs = albums @= albumId
  if Data.IxSet.null albs then
    return []
  else do
    let album = fromJust $ getOne albs
    let trs = tracks @+ Set.toList (albumTracks album)
    return $ toList trs

getTracks :: Query Database [Track]
getTracks = do
  d@Database{..} <- ask
  return $ toList tracks
  
getTrack :: TrackId -> Query Database (Maybe Track)
getTrack trackId = do
  d@Database{..} <- ask
  return $ getOne $ tracks @= trackId

hasFile :: FilePath -> Query Database (Bool)
hasFile file = do
  d@Database{..} <- ask
  return $ isJust $ getOne $ tracks @= TrackFile file


$(makeAcidic ''Database ['addArtist,
                         'getOrCreateArtist,
                         'addAlbum,
                         'getOrCreateAlbum, 
                         'addTrack,
                         'getArtists,
                         'getArtistAlbums,
                         'getArtistTracks,
                         'getAlbums,
                         'getAlbumTracks,
                         'getTracks,
                         'getTrack,
                         'hasFile])
       
openDatabase dir = openLocalStateFrom (dir </> "db") (Database (ArtistId 1) (AlbumId 1) (TrackId 1) Data.IxSet.empty Data.IxSet.empty Data.IxSet.empty)
