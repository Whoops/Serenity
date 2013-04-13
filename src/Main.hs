{-#Language OverloadedStrings, TemplateHaskell #-}
import DB
import Import
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Aeson hiding (json)
import Data.Acid
import Data.FileEmbed
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict)
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
    middleware logStdoutDev
    get "/" $ html $ getStatic "index.html"
    get "/artists" $ do
      artists <- liftIO $ query database GetArtists
      json artists

static :: [(FilePath, Data.ByteString.ByteString)]
static = $(embedDir "src/static")

--getStatic :: FilePath -> T.Text
getStatic path = decodeUtf8 $ fromStrict $ fromJust $ lookup path static