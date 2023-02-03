{-# language OverloadedStrings #-}
module Main where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Network.HTTP.Types as HTTP
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM




data MyState = MyState {
    msCurrentPlayerWhite :: Bool,
    msMoveNumber :: Int}




main :: IO ()
main = do
  mystateVar <- STM.newTVarIO MyState{
    msCurrentPlayerWhite = True, msMoveNumber = 0}
  S.scotty 3000 $ myApp mystateVar

myApp :: STM.TVar MyState -> S.ScottyM ()
myApp mystateVar = do
  S.get "/" $ do
    currentPlayerWhite <- liftIO $ msCurrentPlayerWhite <$> STM.readTVarIO mystateVar
    moveNumber <- liftIO $ msMoveNumber <$> STM.readTVarIO mystateVar
    S.text $ "Hello"


