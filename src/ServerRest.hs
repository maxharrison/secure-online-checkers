module ServerRest where
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Text

app :: Application
app request respond = 
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"

main :: IO ()
main = do
  putStrLn "Server running on port 8080"
  run 8080 app