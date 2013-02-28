import qualified Sound.TagLib as TagLib
import System.Environment (getArgs)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist, createDirectoryIfMissing, copyFile)
import System.FilePath (combine, takeExtension, addExtension, joinPath, takeDirectory)
import Control.Monad (filterM, mfilter)

data Tag = Tag {
  file :: String,
  title :: String,
  artist :: String,
  album :: String,
  genre :: String,
  comment :: String,
  track :: Integer,
  year :: Integer
  } deriving (Show)

main :: IO()
main = do
  args <- getArgs
  inputDir <- canonicalizePath $ head args
  outputDir <- canonicalizePath $ args !! 1
  processDirectory outputDir inputDir

doCopy :: FilePath -> FilePath -> IO ()
doCopy src dest = do
  let dir = takeDirectory dest
  createDirectoryIfMissing True dir
  copyFile src dest
  

processFile :: FilePath -> FilePath -> IO ()
processFile dest file = do
  if takeExtension file == ".mp3" then do
    tag <- extractTag file
    let target = combine dest $ tagPath tag
    doCopy file target
    else return ()

processDirectory :: FilePath -> FilePath -> IO ()
processDirectory dest path = do
  rawContents <- getDirectoryContents path
  let contents = map (combine path) $ filter (`notElem` [".", ".."]) rawContents
  dirs <- filterM doesDirectoryExist contents
  files <- filterM doesFileExist contents
  mapM_ (processDirectory dest) dirs
  mapM_ (processFile dest) files
  
tagPath :: Tag -> FilePath
tagPath tag = let trackTitle = show (track tag) ++ " - " ++ (title tag) 
                  name = addExtension trackTitle "mp3"
                  in
               joinPath [artist tag, album tag, name]

extractTag :: String -> IO Tag
extractTag filename = do
  tagFile <- TagLib.open filename
  tempTag <- extractTagFile filename tagFile
  tag <- constructTag filename tempTag
  return tag
  
extractTagFile :: String -> (Maybe TagLib.TagFile) -> IO (Maybe TagLib.Tag)
extractTagFile _ (Just file) = TagLib.tag file
extractTagFile filename Nothing = error ("Unable to open file: " ++ filename)

constructTag :: String -> (Maybe TagLib.Tag) -> IO Tag
constructTag filename Nothing = error ("Unable to open file: " ++ filename)
constructTag filename (Just tag) = do
  let file = filename
  title <- TagLib.title tag
  artist <- TagLib.artist tag
  album <- TagLib.album tag
  genre <- TagLib.genre tag
  comment <- TagLib.comment tag
  track <- TagLib.track tag
  year <- TagLib.year tag
  return  Tag { file = file,
                title = title,
                artist = artist,
                album = album,
                genre = genre,
                comment = comment,
                track = track,
                year = year }
    
