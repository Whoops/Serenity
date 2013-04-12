{-#Language OverloadedStrings #-}
import DB
import Import
import Web.Scotty
import Data.Aeson hiding (json)
import Data.Acid
import System.Directory (canonicalizePath)
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)

data Flag = Import FilePath
            deriving(Show)

options :: [OptDescr Flag]
options = [ Option ['i'] ["import"] (ReqArg Import "DIR") "directory to import from" ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    ([Import inputDir], [], []) -> importDirectory inputDir
    ([], [], []) -> runServer
    (_, _, errors) -> ioError (userError (concat errors ++ usageInfo header options))
  where header = "Usage: Serenity [OPTION...]"

importDirectory inputDir = do
  database <- openDatabase
  canonicalizePath inputDir >>= processDirectory database
  
runServer = do
  database <- openDatabase
  scotty 3000 $ do
    get "/" $ html "<html><head><title>Serenity</title></head><body>placeholder here</body></html>"
    get "/artists" $ do
      artists <- liftIO $ query database GetArtists
      json artists
