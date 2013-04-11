{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Import where
import DB hiding (main)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Acid
import Data.Aeson
import Audio.TagLib.TagLib
import Data.ByteString.Char8 (pack)

processDirectory database path = do
  dirs >>= mapM_ (processDirectory database)
  files >>= mapM_ (processFile database)
  return ()
  where rawContents = getDirectoryContents path
        contents = rawContents >>= return . map (combine path) . filter (`notElem` [".", ".."])
        dirs = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist
  
extractTag filename = do tagFile <- tagFileOpen $ pack filename
                         tag <- tagFileGetTag $ fromJust tagFile
                         return $ fromJust tag

--processFile :: FilePath -> IO ()
processFile database path = when (takeExtension path == ".mp3") $ do
                            putStrLn path 
                            tag <- extractTag path
                            insertFile database path tag
                            return ()

--insertFile :: FilePath -> TagLib.Tag -> IO ()
insertFile database file tag = do
  artist <- tagGetArtist tag
  album <- tagGetAlbum tag
  title <- tagGetTitle tag
  genre <- tagGetGenre tag
  comment <- tagGetComment tag
  track <- tagGetTrack tag
  year <- tagGetYear tag
  update database (AddTrack file artist album title genre comment (toInteger track) (toInteger year))
  return ()