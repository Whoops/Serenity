{-#Language OverloadedStrings #-}
import DB hiding (main)
import Web.Scotty

main = scotty 3000 $ do
  get "/" $ do
    text "Hello World!"