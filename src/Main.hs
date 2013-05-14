{-#Language OverloadedStrings #-}
import Serenity.DB as DB
import Serenity.Import
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Data.Aeson hiding (json)
import Data.Acid
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

import Paths_Serenity

data Flag = Import FilePath |
            Static FilePath
            deriving(Show)

options :: [OptDescr Flag]
options = [ Option "i" ["import"] (ReqArg Import "DIR") "directory to import from", 
            Option "s" ["static"] (ReqArg Static "DIR") "serve static files from DIR instead of built-ins" ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    ([Import inputDir], [], []) -> importDirectory inputDir
    ([Static staticDir], [], []) -> runServer staticDir
    ([], [], []) -> getDataDir >>= runServer
    (_, _, errors) -> ioError (userError (concat errors ++ usageInfo header options))
  where header = "Usage: Serenity [OPTION...]"

importDirectory inputDir = do
  database <- openDatabase
  canonicalizePath inputDir >>= processDirectory database
  
runServer staticDir = do
  database <- openDatabase
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ noDots >-> addBase staticDir
    get "/" $ redirect "index.html"
    get "/artists" $ do
      artists <- liftIO $ query database GetArtists
      json artists
    get "/artists/:artistId/albums" $ do
      artistId <- param "artistId"
      albums <- liftIO $ query database (GetArtistAlbums $ ArtistId artistId)
      json albums
    get "/artists/:artistId/tracks" $ do
      artistId <- param "artistId"
      tracks <- liftIO $ query database (GetArtistTracks $ ArtistId artistId)
      json tracks
    get "/albums" $ do
      albums <- liftIO $ query database GetAlbums
      json albums
    get "/albums/:albumId/tracks" $ do
      albumId <- param "albumId"
      tracks <- liftIO $ query database (GetAlbumTracks $ AlbumId albumId)
      json tracks
    get "/tracks" $ do
      tracks <- liftIO $ query database GetTracks
      json tracks
    get "/tracks/:trackId" $ do
      trackId <- param "trackId"
      mTrack <- liftIO $ query database (GetTrack $ TrackId trackId)
      case mTrack of
        Just track -> do
          header "Content-Type" "audio/mpeg"
          Web.Scotty.file $ DB.file track
        Nothing -> next
