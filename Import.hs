{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

import DB hiding (main)
import Database.Persist
import Database.Persist.Sqlite
import System.Environment (getArgs)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Maybe (fromJust)
import qualified Sound.TagLib as TagLib


-- Note: Monad type is wrong, sould be something like
-- SqlPersist (Control.Monad.Logger.NoLoggingT (Control.Monad.Trans.Resource.ResourceT IO)) ()
-- automatically derived via runSqlite
--main :: IO ()
main = do
  args <- getArgs
  inputDir <- canonicalizePath $ head args
  runSqlite "db.sqlite" $ do
    processDirectory inputDir
  
--processDirectory :: FilePath -> IO ()
processDirectory path = do
  dirs <- getDirectories path
  mapM_ processDirectory dirs 
  --rawContents <- runResourceT $ getDirectoryContents path
  --let contents = map (combine path) $ filter (`notElem` [".", ".."]) rawContents
  --dirs <- filterM doesDirectoryExist contents
  --files <- filterM doesFileExist contents
  --mapM_ processDirectory dirs
  --mapM_ processFile files
  return ()
  
getDirectories :: FilePath -> IO [FilePath]
getDirectories path = do
  rawContents <- getDirectoryContents path
  let contents = map (combine path) $ filter (`notElem` [".", ".."]) rawContents
  filterM doesDirectoryExist contents
  
--extractTag :: String -> IO TagLib.Tag
extractTag filename = do tagFile <- TagLib.open filename
                         tag <- TagLib.tag $ fromJust tagFile
                         return $ fromJust tag

--processFile :: FilePath -> IO ()
--processFile path = when (takeExtension path == "mp3") $
--                   do tag <- extractTag path
--                      return ()

--insertFile :: FilePath -> TagLib.Tag -> IO ()
--insertFile file tag = do
--  artistName <- TagLib.artist tag
--  albumName <- TagLib.album tag
  --artistId <-  getOrCreateArtist artistName
  --albumId <- getOrCreateAlbum albumName artistId
--  return ()


--getOrCreateArtist :: String -> IO ArtistId
getOrCreateArtist name = do
  maybeArtist <- selectFirst [ArtistName ==. name] []
  case maybeArtist of
    Just artist -> return $ entityKey artist
    Nothing -> insert $ Artist name

getOrCreateAlbum name artistId = do
  maybeAlbum <- selectFirst [AlbumName ==. name] []
  case maybeAlbum of
    Just album -> return $ entityKey album
    Nothing -> insert $ Album name artistId