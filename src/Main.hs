{-# language OverloadedStrings #-}
module Main where

-- Haskell modules
import qualified Web.Scotty as S
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8BS
import Network.HTTP.Types.Status (badRequest400, ok200)
import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import Data.Bits (shiftR, (.&.))
import Data.Maybe (fromJust)

-- My modules
import GameState
import Parsers
import Valid
import AI
import DES
import ECDH

-- run instructions in README.md

----------------------------------------------------------------
--                    Command Type and Parser                 --
----------------------------------------------------------------


type ID = String
type AI = Bool
data Command = Poll ID Nonce | Start ID AI Point | Move ID Route
    deriving (Eq, Show)

  
commandParser :: Parser Command
commandParser = do
    stringToken "poll"
    space
    id <- ident
    nonce <- nonceParser
    return (Poll id nonce)
    <|> do 
        stringToken "start"
        space
        id <- ident
        ai <- boolParser
        otherPublicKey <- publicKeyParser
        return (Start id ai otherPublicKey)
    <|> do
        stringToken "move"
        space
        id <- ident
        route <- routeParser
        return (Move id route)


nonceParser :: Parser Word
nonceParser = token $ do
  nonce <- natural
  return $ fromIntegral nonce


publicKeyParser :: Parser Point
publicKeyParser = token $ do
  number <- natural
  return $ fromJust $ integerToPublicKey number


parseCommand :: String -> Maybe Command
parseCommand s =
  case (parse commandParser s) of
    [(c, "")] -> Just c
    _         -> Nothing



--------------------------------------------------------------------------------
--                               Key Exhcnage                                 --
--------------------------------------------------------------------------------


derive64BitKey :: Integer -> Word
derive64BitKey n = fromIntegral $ n .&. 0xFFFFFFFFFFFFFFFF


ecdh :: MV.MVar GameState -> Point -> S.ActionM Word
ecdh gameStateVar otherPublicKey = do
  -- Generate the public and private keys
  (privateKey, Point x y) <- liftIO generateKeyPair_Secp256k1

  -- send the public key
  let publicKeyBytes = BS.concat [BS.singleton 4, intToBS x, intToBS y]
  sendData publicKeyBytes

  -- calculate the shared secret and derive the key
  let shared_secret = sharedSecret_Secp256k1 privateKey otherPublicKey
  let key = derive64BitKey shared_secret
  return key


-- This function is explained backwards because of how it is written.
-- fmap is used to right shift each number in the list by n. This list is
-- from 248 to zero in 8 step decrements - this means there are 32 items.
-- Then each item in the list is masked with 255. Then finally, BS.pack
-- takes 32 integer values and converts them to a bytestring.
intToBS :: Integer -> BS.ByteString
intToBS n = BS.pack $
  fmap (fromInteger . (255 .&.)) $
  fmap (shiftR n) [248, 240 .. 0]



--------------------------------------------------------------------------------
--                               Start Function                               --
--------------------------------------------------------------------------------


-- | 'start' initialises a new game session based on the input parameters.
-- If AI = True, it assigns the 'id' to the white player and AI to black player.
-- If AI = False, it assigns the 'id' to the first available player slot.
start :: MV.MVar GameState -- ^ The GameState MVar holding the current state
      -> ID                -- ^ The string ID of the request
      -> AI                -- ^ Bool of whether the opponent should be AI.
      -> Point             -- ^ The ECDH public key of the client.
      -> S.ActionM ()

-- With AI
start gameStateVar id True otherPublicKey = do
  -- Get the white player ID from the game state.
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  
  -- If no white player yet, set current player as white and AI as black.
  if whiteID == Nothing
    then do
      key <- ecdh gameStateVar otherPublicKey
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        whiteID = Just id,
        whiteKey = key,
        blackID = Just "AI",
        ai = True,
        started = True})
      
    -- Send a bad request response if white player is already set.
    else sendInvalid


-- Without AI - i.e. multiplayer
start gameStateVar id False otherPublicKey = do
  -- Get the white and black player IDs from the game state.
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
  
  -- Assign the current player to the first available player slot.
  case (whiteID, blackID) of

    -- If there is no white player, set the current player as white.
    (Nothing, _      ) -> do
      key <- ecdh gameStateVar otherPublicKey
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        whiteID = Just id,
        whiteKey = key})

    -- If there is no black player, set current player as black
    --  and mark the game as started.
    (_      , Nothing) -> do
      key <- ecdh gameStateVar otherPublicKey
      liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
        blackID = Just id,
        blackKey = key,
        started = True})

    -- Send a bad request response if both player slots are already filled.
    _ -> sendInvalid



--------------------------------------------------------------------------------
--                               Move Functions                               --
--------------------------------------------------------------------------------


won :: Board -> String
won board =
  let whiteMoves = getAllValidBoardsRoutes board White
      blackMoves = getAllValidBoardsRoutes board Black
  in case (whiteMoves, blackMoves) of
    ([], []) -> "D" -- Draw
    ([], _)  -> "B" -- Black wins
    (_, [])  -> "W" -- White wins
    _        -> "P" -- Still playing


sendGameOver :: S.ActionM ()
sendGameOver = do
  liftIO $ putStrLn "Game Over"
  sendData "Game Over"
  

sendInvalid :: S.ActionM ()
sendInvalid = do
  liftIO $ putStrLn "Invalid"
  S.status badRequest400


nextPlayer :: MV.MVar GameState -> S.ActionM ()
nextPlayer gameStateVar = do
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar 
  liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
    currentPlayer = if currentPlayer == White then Black else White})


updateBoard :: MV.MVar GameState -> Board -> S.ActionM ()
updateBoard gameStateVar board = do
  liftIO $ MV.modifyMVar_ gameStateVar (\gs -> return gs {
    board = board})


moveAI :: MV.MVar GameState -> S.ActionM ()
moveAI gameStateVar = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  case (best_move 6 score_count board Black) of
    Just (newBoard, _, _) -> do
      updateBoard gameStateVar newBoard
    Nothing               -> liftIO $ putStrLn "NO POSSIBLE MOVES: GAME ENDED"


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
  -- Retrieve the current board and player from the GameState
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar

  -- Get all valid routes for the current player
  let routes = getAllValidBoardsRoutes board currentPlayer

  -- Check if the move is valid
  if route `elem` (map snd routes)
    then do
      -- Update the board with the new move
      let newBoard = fst $ head $ filter (\(_, r) -> r == route) routes
      updateBoard gameStateVar newBoard

      -- Send the status
      S.status ok200

      -- Proceed with either an AI move or the next player's move
      ai <- liftIO $ GameState.ai <$> MV.readMVar gameStateVar
      if ai
        then moveAI gameStateVar
        else nextPlayer gameStateVar

    else do
      -- If the move is invalid, print a message and return a "Bad Request"
      liftIO $ putStrLn "Invalid move"
      sendInvalid

  
-- | This function tries to indentify a player from the ID given
identify :: MV.MVar GameState -- ^ The GameState MVar holding the current state
         -> ID                -- ^ The string ID to match
         -> S.ActionM (Maybe Player)
identify gameStateVar id = do
  whiteID <- liftIO $ GameState.whiteID <$> MV.readMVar gameStateVar
  blackID <- liftIO $ GameState.blackID <$> MV.readMVar gameStateVar
  return $ if Just id == whiteID then Just White
    else if Just id == blackID then Just Black
      else Nothing


-- | This function says whether a player given is the current player
checkCurrentPlayer :: MV.MVar GameState -> Maybe Player -> S.ActionM (Bool)
checkCurrentPlayer gameStateVar maybePlayer = do
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  return $ if maybePlayer == Just currentPlayer then True else False



----------------------------------------------------------------
--                  Sending and Encryption                    --
----------------------------------------------------------------


poll :: MV.MVar GameState -> Player -> Nonce -> S.ActionM ()
poll gameStateVar player nonce = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  sendBoard gameStateVar player nonce


sendBoard :: MV.MVar GameState -> Player -> Nonce -> S.ActionM ()
sendBoard gameStateVar player nonce = do
  board <- liftIO $ GameState.board <$> MV.readMVar gameStateVar
  currentPlayer <- liftIO $ GameState.currentPlayer <$> MV.readMVar gameStateVar
  let winner = " " ++ won board ++ " "
  let string = show player ++ winner ++ showFullBoard board currentPlayer
  sendEncryptedData gameStateVar player string nonce


sendEncryptedData :: MV.MVar GameState -> Player -> String -> Nonce -> S.ActionM ()
sendEncryptedData gameStateVar player string nonce = do
  let playerKey = if player == White then GameState.whiteKey else GameState.blackKey
  key <- liftIO $ playerKey <$> MV.readMVar gameStateVar
  let ciphertext = encrypt key nonce (stringToBytes string)
  let ciphertextByteString = BS.pack $ map fromIntegral ciphertext
  sendData ciphertextByteString


sendData :: BS.ByteString -> S.ActionM ()
sendData = S.raw . LBS.fromStrict



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
    whiteKey = 0,
    blackID = Nothing,
    blackKey = 0,
    ai = False
  }

  -- start the web server
  S.scotty 3000 $ do

    -- curl -X POST -H "Content-Type: application/octet-stream" --data-binary $'poll' localhost:3000/binary
    S.post "/binary" $ do
      body <- S.body

      case parseCommand $ C8BS.unpack (LBS.toStrict body) of

        -- invalid command
        Nothing -> sendInvalid

        -- poll
        Just (Poll id nonce) -> do
          player <- identify gameStateVar id
          case player of
            Nothing -> sendInvalid
            Just p -> poll gameStateVar p nonce

        -- start
        Just (Start id ai otherPublicKey) -> start gameStateVar id ai otherPublicKey

        -- move
        Just (Move id route) -> do
          checkStarted <- liftIO $ GameState.started <$> MV.readMVar gameStateVar
          playerWhoIsConnected <- identify gameStateVar id
          checkCurrentPlayer <- checkCurrentPlayer gameStateVar playerWhoIsConnected
          case (checkStarted, playerWhoIsConnected, checkCurrentPlayer) of
            (True, Just p, True)  -> move gameStateVar p route
            otherwise             -> sendInvalid