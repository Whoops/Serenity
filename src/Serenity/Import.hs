{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Serenity.Import where
import Serenity.DB
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Acid
import Data.Aeson
import Audio.TagLib as TL
--import qualified Sound.TagLib as TL

processDirectory :: AcidState Database -> FilePath -> IO (Integer, Integer)
processDirectory database path = runStateT (doDirectory database path) (0, 0) >>= return . snd

doDirectory :: AcidState Database -> FilePath -> StateT (Integer, Integer) IO ()
doDirectory database path = do
  liftIO dirs >>= mapM_ (doDirectory database)
  liftIO files >>= mapM_ (processFile database)
  return ()
  where rawContents = getDirectoryContents path
        contents = liftM (map (combine path) . filter (`notElem` [".", ".."])) rawContents
        dirs = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist

--extractTag :: FilePath -> IO TL.Tag
--extractTag filename = do tagFile <- TL.open filename
--                         tag <- TL.tag $ fromJust tagFile
--                         return $ fromJust tag

processFile :: AcidState Database -> FilePath -> StateT (Integer, Integer) IO ()
processFile database path = when (takeExtension path == ".mp3") $ do
  (new, old) <- get
  has <- liftIO $ query database (HasFile path)
  if has
     then put (new, old + 1)
    else do
    liftIO $ putStrLn path 
    liftIO $ insertFile database path
    put (new + 1, old)
                            
insertFile :: AcidState Database -> FilePath -> IO ()
insertFile database file = do
  taglib $ do
    tag <- TL.openFile file
    artist <- getArtist tag
    album <- getAlbum tag
    title <- getTitle tag
    genre <- getGenre tag
    comment <- getComment tag
    track <- TL.getTrack tag
    year <- getYear tag
    io $ update database (AddTrack file artist album title genre comment track year)
    return ()
    
  --tagFile <- TL.open file
  --mTag <- TL.tag $ fromJust tagFile
  --let tag = fromJust mTag
  --artist <- TL.artist tag
  --album <- TL.album tag
  --title <- TL.title tag
  --genre <- TL.genre tag
  --comment <- TL.comment tag
  --track <- TL.track tag
  --year <- TL.year tag
  --update database (AddTrack file artist album title genre comment track year)
  --TL.close $ fromJust tagFile
