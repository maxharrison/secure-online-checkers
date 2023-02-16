{-# language OverloadedStrings #-}
module Main where

import qualified Web.Scotty as S
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8BS



-- run instructions in README.md



data State = State {
    sCounter :: Int }





main :: IO ()
main = do

  -- initialise the state mvar
  stateVar <- MV.newMVar State {
    sCounter = 0 }

  -- start the web server
  S.scotty 3000 $ do

    -- curl localhost:3000/poll
    S.get "/poll" $ do
      S.setHeader "Content-Type" "application/octet-stream" -- Set the content type to indicate binary data
      S.raw $ LBS.fromStrict $ C8BS.pack ("poll\n")

    -- curl -X POST -H "Content-Type: application/octet-stream" --data-binary $'hello' localhost:3000/binary
    S.post "/binary" $ do
      body <- S.body
      liftIO $ putStrLn $ "body: " ++ show body

      counter <- liftIO $ sCounter <$> MV.takeMVar stateVar
      liftIO $ MV.putMVar stateVar State {
        sCounter = counter + 1 }

      S.setHeader "Content-Type" "application/octet-stream" -- Set the content type to indicate binary data
      S.raw $ LBS.fromStrict $ C8BS.pack (show counter ++ "\n")
    

