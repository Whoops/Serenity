{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import DB hiding (main)
import System.Environment (getArgs)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Acid
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Sound.TagLib as TagLib

main :: IO ()
main = do
  args <- getArgs
  inputDir <- canonicalizePath $ head args
  database <- openDatabase
  processDirectory database inputDir
  (query database GetArtists) >>= B.putStrLn . encode
  (query database GetAlbums) >>= B.putStrLn . encode
  (query database GetTracks) >>= B.putStrLn . encode
  
processDirectory database path = do
  dirs >>= mapM_ (processDirectory database)
  files >>= mapM_ (processFile database)
  return ()
  where rawContents = getDirectoryContents path
        contents = rawContents >>= return . map (combine path) . filter (`notElem` [".", ".."])
        dirs = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist
  
extractTag filename = do tagFile <- TagLib.open filename
                         tag <- TagLib.tag $ fromJust tagFile
                         return $ fromJust tag

--processFile :: FilePath -> IO ()
processFile database path = when (takeExtension path == ".mp3") $ do
                            putStrLn path 
                            tag <- extractTag path
                            insertFile database path tag
                            return ()

--insertFile :: FilePath -> TagLib.Tag -> IO ()
insertFile database file tag = do
  artist <- TagLib.artist tag
  album <- TagLib.album tag
  title <- TagLib.title tag
  genre <- TagLib.genre tag
  comment <- TagLib.comment tag
  track <- TagLib.track tag
  year <- TagLib.year tag
  update database (AddTrack file artist album title genre comment track year)
  return ()