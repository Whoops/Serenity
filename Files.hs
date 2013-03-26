{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getArgs)
import System.Directory (canonicalizePath, getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (combine, takeExtension)
import Control.Monad (filterM, mapM_, liftM)
import Control.Monad.IO.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
File                                                       
  path String
  deriving (Show)
|]
main :: IO ()
main = do
  args <- getArgs
  path <- canonicalizePath $ head args
  runSqlite "files.sqlite" $ do
    runMigration migrateAll
    processDirectory path
    return ()

processDirectory path = liftIO files >>= mapM_ processFile >>
                        liftIO directories >>= mapM_ processDirectory
  where contents  = getDirectoryContents path >>=
                    return . map (combine path) . filter (`notElem` [".", ".."]) 
        directories = contents >>= filterM doesDirectoryExist
        files = contents >>= filterM doesFileExist
        
processFile path = insert $ File path