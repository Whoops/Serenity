{-#Language OverloadedStrings, TemplateHaskell#-}
import Serenity.DB as DB
import Serenity.Import

import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.List (find)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getHomeDirectory)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath

import Data.FileEmbed (embedDir)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Data.Aeson hiding (json)
import Data.Acid
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

data Flag = Import FilePath |
            Setup |
            Run
            deriving(Show, Eq)

statics :: [(FilePath, BS.ByteString)]
statics = $(embedDir "static")

options :: [OptDescr Flag]
options = [ Option "i" ["import"] (ReqArg Import "DIR") "Import mp3s from DIR into database in RUNDIR", 
            Option "s" ["setup"] (NoArg Setup) "setup database and static files in RUNDIR" ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    ([], [], []) -> defaultDirectory >>= runServer
    ([], [dir], []) -> runServer dir
    (args, [dir], []) -> runArgs args dir
    (args, _, []) -> defaultDirectory >>= runArgs args
    (_, _, errors) -> ioError (userError (concat errors ++ usageInfo header options))
  where header = "Usage: Serenity [OPTION...] [RUNDIR]"

defaultDirectory :: IO String
defaultDirectory = do
  home <- getHomeDirectory
  return (home </> ".Serenity")

runArgs :: [Flag] -> FilePath -> IO ()
runArgs args dir = do
  when (Setup `elem` args) $ setupDir dir
  let imp = find isImport args
  maybe (return ()) (importDirectory dir) imp
  where
    isImport (Import _) = True
    isImport _ = False

setupDir :: FilePath -> IO ()
setupDir dir = do
  createDirectoryIfMissing True dir
  extractStatics (dir </> "static")

extractStatics :: FilePath -> IO ()
extractStatics dir = mapM_ extractFile statics
  where
    extractFile (path, contents) = do
      createDirectoryIfMissing True (dir </> (takeDirectory path))
      BS.writeFile (dir </> path) contents      

importDirectory workDir (Import inputDir) = do
  database <- openDatabase workDir
  (new, old) <- canonicalizePath inputDir >>= processDirectory database
  putStrLn $ "Imported " ++ show new ++ " files"
  putStrLn $ "Ignored " ++ show old ++ " already imported files"
  closeAcidState database
  
runServer workDir = do
  database <- openDatabase workDir
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ noDots >-> addBase (workDir </> "static")
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
          setHeader "Content-Type" "audio/mpeg"
          Web.Scotty.file $ DB.file track
        Nothing -> next
