{-#Language OverloadedStrings #-}
import DB
import Import
import Web.Scotty
import Data.Aeson hiding (json)
import Data.Acid
import System.Environment (getArgs)
import System.Directory (canonicalizePath)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

main :: IO ()
main = do
  args <- getArgs
  database <- openDatabase
  inputDir <- canonicalizePath $ head args
  processDirectory database inputDir
  (query database GetArtists) >>= B.putStrLn . encode
  (query database GetAlbums) >>= B.putStrLn . encode
  (query database GetTracks) >>= B.putStrLn . encode
  
--main = scotty 3000 $ do
--  get "/" $ do
--    json artists