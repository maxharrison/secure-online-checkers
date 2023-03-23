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
import Control.Applicative ((<|>))


-- My modules
import GameState
import Parsers
import Valid
import AI
import DES
import Binary


-- run instructions in README.md

----------------------------------------------------------------
--                    Command Type and Parser                 --
----------------------------------------------------------------

type ID = String
type AI = Bool
data Command = Poll ID | Start ID AI | Move ID Route
    deriving (Eq, Show)

  
commandParser :: Parser Command
commandParser = do
    stringToken "poll"
    space
    id <- ident
    return (Poll id)
    <|> do 
        stringToken "start"
        space
        id <- ident
        ai <- boolParser
        return (Start id ai)
    <|> do
        stringToken "move"
        space
        id <- ident
        route <- routeParser
        return (Move id route)



parseCommand :: String -> Maybe Command
parseCommand s =
  case (parse commandParser s) of
    [(c, "")] -> Just c
    _         -> Nothing

--------------------------------------------------------------------------------
--                               Start Function                               --
--------------------------------------------------------------------------------




-- | 'start' initializes a new game session based on the input parameters.
-- If AI = True, it assigns the 'id' to the white player and AI to black player.
-- If AI = False, it assigns the 'id' to the first available player slot.
start :: MV.MVar GameState -- ^ The GameState MVar holding the current state
      -> ID                -- ^ The string ID of the request
      -> AI                -- ^ Bool of whether the opponent should be AI.
      -> S.ActionM ()
start gameStateVar id True = do
  -- Get the white player ID from the game state.
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  
  -- If no white player yet, set current player as white and AI as black.
  if whiteID == Nothing
    then liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
           whiteID = Just id,
           blackID = Just "AI",
           ai = True,
           started = True
           })
    -- Send a bad request response if white player is already set.
    else S.status badRequest400


start gameStateVar id False = do
  -- Get the white and black player IDs from the game state.
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
  
  -- Assign the current player to the first available player slot.
  case (whiteID, blackID) of
    (Nothing, _      ) -> do
      -- If there is no white player, set the current player as white.
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        whiteID = Just id
      })
    (_      , Nothing) -> do
      -- If there is no black player, set current player as black
      --  and mark the game as started.
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        blackID = Just id,
        started = True
      })
    -- Send a bad request response if both player slots are already filled.
    _ -> S.status badRequest400












--------------------------------------------------------------------------------
--                               Move Functions                               --
--------------------------------------------------------------------------------


nextPlayer :: MV.MVar GameState -> S.ActionM ()
nextPlayer gameStateVar = do
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar 
  liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
    currentPlayer = if currentPlayer == White then Black else White
    })

updateBoard :: MV.MVar GameState -> Board -> S.ActionM ()
updateBoard gameStateVar board = do
  liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
    board = board
    })


moveAI :: MV.MVar GameState -> S.ActionM ()
moveAI gameStateVar = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  case (best_move 3 score_count board Black) of
    Just (newBoard, _, _) -> do
      updateBoard gameStateVar newBoard
      sendBoard gameStateVar White
    Nothing            -> liftIO $ putStrLn "NO POSSIBLE MOVES: GAME ENDED"




-- | This function manages a move in a client-server checkers game. It first
-- retrieves the current board and player from the GameState. Then, it checks if
-- the move is valid. If valid, it updates the board, sends the board to the
-- client, and proceeds with either an AI move or the next player's move. If
-- invalid, it returns a "Bad Request" response.
move :: MV.MVar GameState -- ^ The GameState MVar holding the current state
     -> Player            -- ^ The player making the move
     -> Route             -- ^ The move to be made
     -> S.ActionM ()
move gameStateVar player route = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  let routes = getAllValidBoardsRoutes board currentPlayer
  ai <- liftIO $ GameState.ai <$> MV.readMVar gameStateVar

  if route `elem` (map snd routes)

    then do
      let newBoard = fst $ head $ filter (\(_, r) -> r == route) routes
      updateBoard gameStateVar newBoard
      sendBoard gameStateVar currentPlayer

      if ai
        then moveAI gameStateVar
        else nextPlayer gameStateVar

    else do
      liftIO $ putStrLn "Invalid move"
      S.status badRequest400

  

identify :: MV.MVar GameState -> ID -> S.ActionM (Maybe Player)
identify gameStateVar id = do
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
  return $ if Just id == whiteID then Just White
    else if Just id == blackID then Just Black
      else Nothing

checkCurrentPlayer :: MV.MVar GameState -> Maybe Player -> S.ActionM (Bool)
checkCurrentPlayer gameStateVar maybePlayer = do
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  return $ if maybePlayer == Just currentPlayer then True else False



----------------------------------------------------------------
--                  Sending and Encryption                    --
----------------------------------------------------------------

poll :: MV.MVar GameState -> Player -> S.ActionM ()
poll gameStateVar player = sendBoard gameStateVar player

sendBoard :: MV.MVar GameState -> Player -> S.ActionM ()
sendBoard gameStateVar player = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  sendData gameStateVar player $ showFullBoard board currentPlayer

sendData :: MV.MVar GameState -> Player -> String -> S.ActionM ()
sendData gameStateVar player string = do
  let playerKey = if player == White then GameState.whiteKey else GameState.blackKey
  key <- liftIO $ playerKey <$> MV.readMVar gameStateVar
  let nonce = replicate 32 False
  let ciphertext = (encrypt key nonce. stringToBinary) string
  let ciphertextString = showBinary ciphertext
  S.setHeader "Content-Type" "application/octet-stream"
  S.raw $ LBS.fromStrict $ C8BS.pack ciphertextString


----------------------------------------------------------------
--                  Main Function and Routes                  --
----------------------------------------------------------------


main :: IO ()
main = do

  -- initialise the state mvar
  gameStateVar <- MV.newMVar GameState {
    board = startBoard,
    currentPlayer = White,
    started = False,
    whiteID = Nothing,
    whiteKey = map (\c -> c=='1') "0110100101110111011100100111001101101110011001100110100001101100",
    blackID = Nothing,
    blackKey = map (\c -> c=='1') "0110100101110111011100100111001101101110011001100110100001101100",
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
        Just (Poll id) -> do
          player <- identify gameStateVar id
          case player of
            Nothing -> S.status badRequest400
            Just p -> poll gameStateVar p

        -- start
        Just (Start id ai) -> start gameStateVar id ai

        -- move
        Just (Move id route) -> do
          checkStarted <- liftIO $ GameState.started <$> MV.readMVar gameStateVar
          playerWhoIsConnected <- identify gameStateVar id
          checkCurrentPlayer <- checkCurrentPlayer gameStateVar playerWhoIsConnected
          case (checkStarted, playerWhoIsConnected, checkCurrentPlayer) of
            (True, Just p, True)  -> move gameStateVar p route
            otherwise             -> S.status badRequest400





