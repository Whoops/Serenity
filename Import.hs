{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import DB hiding (main)
import Database.Persist
import Database.Persist.Sqlite
import System.Environment (getArgs)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Sound.TagLib as TagLib

main :: IO ()
main = do
  args <- getArgs
  inputDir <- canonicalizePath $ head args
  runSqlite "db.sqlite" $ do
    processDirectory inputDir
  
processDirectory path = do
  liftIO dirs >>= mapM_ processDirectory
  liftIO files >>= mapM_ processFile
  return ()
  where rawContents = getDirectoryContents path
        contents = rawContents >>= return . map (combine path) . filter (`notElem` [".", ".."])
        dirs = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist
  
extractTag filename = do tagFile <- TagLib.open filename
                         tag <- TagLib.tag $ fromJust tagFile
                         return $ fromJust tag

--processFile :: FilePath -> IO ()
processFile path = when (takeExtension path == ".mp3") $
                   do liftIO $ putStrLn path 
                      liftIO tag >>= insertFile path
                      return ()
                   where tag = extractTag path

--insertFile :: FilePath -> TagLib.Tag -> IO ()
insertFile file tag = do
  artistId <-  liftIO artistName >>= getOrCreateArtist
  albumId <- liftIO albumName >>= getOrCreateAlbum
  (liftIO $ createTrack file artistId albumId tag) >>= insert
  return ()
  where artistName = TagLib.artist tag
        albumName = TagLib.album tag
        
createTrack file artist album tag = do
  title <- TagLib.title tag
  genre <- TagLib.genre tag
  comment <- TagLib.comment tag
  track <- TagLib.track tag
  year <- TagLib.year tag
  return $ Track file title artist album genre comment (fromIntegral track) (fromIntegral year)

--getOrCreateArtist :: String -> IO ArtistId
getOrCreateArtist name = do
  maybeArtist <- selectFirst [ArtistName ==. name] []
  case maybeArtist of
    Just artist -> return $ entityKey artist
    Nothing -> insert $ Artist name

getOrCreateAlbum name = do
  maybeAlbum <- selectFirst [AlbumName ==. name] []
  case maybeAlbum of
    Just album -> return $ entityKey album
    Nothing -> insert $ Album name