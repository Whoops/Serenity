{-#Language OverloadedStrings #-}
import DB hiding (main)
import Web.Scotty
--import Database.Persist
--import Database.Persist.Sqlite
import Data.Aeson hiding (json)
--import Control.Monad.IO.Class (liftIO)

artists = Artist "John Doe"

main = scotty 3000 $ do
  get "/" $ do
    json artists

--main = runSqlite "db.sqlite" $ do 
--  liftIO $ scotty 3000 $ do
--  Web.Scotty.get "/" $ do
--    lift $ selectList [ArtistName !=. "joe"] []
--    text "hello world!"