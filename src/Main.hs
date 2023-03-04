{-# language OverloadedStrings #-}
module Main where

-- Haskell modules
import qualified Web.Scotty as S
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8BS

import Network.HTTP.Types.Status (badRequest400)


import Debug.Trace

-- My modules
import GameState
import Parsers
import Valid
import AI
import DES
import Binary

-- run instructions in README.md





parseCommand :: String -> Maybe Command
parseCommand s =
  case (parse commandParser s) of
    [(c, "")] -> Just c
    _         -> Nothing


sendBoard :: MV.MVar GameState -> S.ActionM ()
sendBoard gameStateVar = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  sendData $ showFullBoard board currentPlayer


poll :: MV.MVar GameState -> S.ActionM ()
poll gameStateVar = sendBoard gameStateVar



startAI :: MV.MVar GameState -> String -> S.ActionM ()
startAI gameStateVar id =
  liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
    whiteID = Just id,
    started = True,
    blackID = Just "AI",
    ai = True })



start :: MV.MVar GameState -> String -> S.ActionM ()
start gameStateVar id = do
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
  case (whiteID, blackID) of
    (Nothing, _      ) -> do
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        whiteID = Just id })
    (_      , Nothing) -> do
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        blackID = Just id,
        started = True })
    _                  -> S.status badRequest400


-- | 'moveAI' performs the same thing as 'move', but instead of waiting for
--   the second player, the second player is the AI, and this calcuates
--   the next best move, and plays it.
moveAI :: MV.MVar GameState -> ID -> Route -> S.ActionM ()
moveAI gameStateVar id route = do
  -- Runs 'move'
  move gameStateVar id route
  -- Then calcuates the next best move using minimax
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  let bestMove = trace (show $ (best_move 5 score_count board Black)) (best_move 5 score_count board Black)
  case (best_move 3 score_count board Black) of
    Just (_, _, routeAI) -> move gameStateVar "AI" routeAI
    Nothing -> move gameStateVar id route







-- | 'move' is a function that takes an 'MV.MVar' that holds the 'GameState', 
--   an 'ID' that represents the player making the move, and a 'Route' that 
--   represents the move to be made. It returns a 'S.ActionM' action that sends 
--   the updated board to the client.
move :: MV.MVar GameState -> ID -> Route -> S.ActionM ()
move gameStateVar id route = do
  -- Check if the game has started.
  started <- liftIO $ GameState.started <$> MV.readMVar gameStateVar
  if not started
    then do
      liftIO $ putStrLn "Game has not started yet"
      S.status badRequest400 -- Send a Bad Request status.
    else do
      -- Get the current board and player from the 'GameState'.
      board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
      currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
      -- Check if the player is valid.
      check <- checkPlayer gameStateVar id
      if not check
        then do
          liftIO $ putStrLn "Invalid player"
          S.status badRequest400 -- Send a Bad Request status.
        else do
          -- Get all the valid moves for the current player.
          let routes = getAllValidBoardsRoutes board currentPlayer
          -- Check if the move is valid.
          if route `elem` (map snd routes)
            then do
              -- Get the new board after the move is made.
              let newBoard = fst $ head $ filter (\(_, r) -> r == route) routes
              -- Update the 'board' and 'currentPlayer' fields in the 'GameState' 'MV.MVar'.
              liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
                board = newBoard,
                currentPlayer = if currentPlayer == White then Black else White })
              -- Send the new board to the client.
              sendBoard gameStateVar
            else do
              liftIO $ putStrLn "Invalid move"
              S.status badRequest400 -- Send a Bad Request status.










checkPlayer :: MV.MVar GameState -> ID -> S.ActionM Bool
checkPlayer gameStateVar id = do
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  if currentPlayer == White
    then do whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
            return $ whiteID == Just id
    else do blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
            return $ blackID == Just id

sendData :: String -> S.ActionM ()
sendData s = do
  let key = stringToBinary "iwrsnfhl"
  let nonce = replicate 32 False
  let ciphertext = (encrypt key nonce. stringToBinary) s
  S.setHeader "Content-Type" "application/octet-stream"
  S.raw $ LBS.fromStrict $ C8BS.pack $ showBinary ciphertext



main :: IO ()
main = do

  -- initialise the state mvar
  gameStateVar <- MV.newMVar GameState {
    board = startBoard,
    currentPlayer = White,
    started = False,
    whiteID = Nothing,
    blackID = Nothing,
    ai = False
  }

  -- start the web server
  S.scotty 3000 $ do

    --S.get "/binary" $ do
    --  sendBoard gameStateVar

    -- curl -X POST -H "Content-Type: application/octet-stream" --data-binary $'poll' localhost:3000/binary
    S.post "/binary" $ do
      body <- S.body
      liftIO $ putStrLn $ "body: " ++ show body

      case parseCommand $ C8BS.unpack (LBS.toStrict body) of

        -- invalid command
        Nothing -> S.status badRequest400

        -- poll
        Just Poll -> poll gameStateVar

        -- start
        Just (Start id ai) ->
          if ai
            then startAI gameStateVar id
            else start gameStateVar id

        -- move
        Just (Move id route) -> do
          ai <- liftIO $ GameState.ai <$> MV.readMVar gameStateVar
          if ai
            then moveAI gameStateVar id route
            else move gameStateVar id route




