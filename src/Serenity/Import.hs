{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Serenity.Import where
import Serenity.DB
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Acid
import Data.Aeson
import qualified Sound.TagLib as TL

processDirectory :: AcidState Database -> FilePath -> IO ()
processDirectory database path = do
  dirs >>= mapM_ (processDirectory database)
  files >>= mapM_ (processFile database)
  return ()
  where rawContents = getDirectoryContents path
        contents = liftM (map (combine path) . filter (`notElem` [".", ".."])) rawContents
        dirs = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist

extractTag :: FilePath -> IO TL.Tag
extractTag filename = do tagFile <- TL.open filename
                         tag <- TL.tag $ fromJust tagFile
                         return $ fromJust tag

processFile :: AcidState Database -> FilePath -> IO ()
processFile database path = when (takeExtension path == ".mp3") $ do
                            putStrLn path 
                            insertFile database path
                            
insertFile :: AcidState Database -> FilePath -> IO ()
insertFile database file = do
  tagFile <- TL.open file
  mTag <- TL.tag $ fromJust tagFile
  let tag = fromJust mTag
  artist <- TL.artist tag
  album <- TL.album tag
  title <- TL.title tag
  genre <- TL.genre tag
  comment <- TL.comment tag
  track <- TL.track tag
  year <- TL.year tag
  update database (AddTrack file artist album title genre comment track year)
  TL.close $ fromJust tagFile
