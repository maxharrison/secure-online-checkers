module GameState where

-- Haskell modules
import qualified Data.Map as Map

-- My modules
import State



size :: Int
size = 8

data Player = White | Black
    deriving (Eq, Show)

data PieceType = Pawn | King
    deriving (Eq, Show)

type Position = (Int, Int)

type Board = Map.Map Position (Player, PieceType)

data GameState = GameState {
  board         :: Board,  
  currentPlayer :: Player,   
  moveNumber    :: Integer
}

type Route = [Position]

getBoard :: STIO GameState Board
getBoard = do
    gameState <- stState
    return (board gameState)

getCurrentPlayer :: STIO GameState Player
getCurrentPlayer = do
    gameState <- stState
    return (currentPlayer gameState)

getMoveNumber :: STIO GameState Integer
getMoveNumber = do
    gameState <- stState
    return (moveNumber gameState)

setBoard :: Board -> STIO GameState ()
setBoard newBoard = do
    gameState <- stState
    stUpdate gameState {board = newBoard}

setCurrentPlayer :: Player -> STIO GameState ()
setCurrentPlayer newPlayer = do
    gameState <- stState
    stUpdate gameState {currentPlayer = newPlayer}

setMoveNumber :: Integer -> STIO GameState ()
setMoveNumber newMoveNumber = do
    gameState <- stState
    stUpdate gameState {moveNumber = newMoveNumber}

getPiece :: Position -> STIO GameState (Maybe (Player, PieceType))
getPiece position = do
    board <- getBoard
    return $ Map.lookup position board

insertPiece :: Position -> (Player, PieceType) -> STIO GameState ()
insertPiece position (player, pieceType) = do
    board <- getBoard
    setBoard $ Map.insert position (player, pieceType) board

deletePiece :: Position -> STIO GameState ()
deletePiece position = do
    board <- getBoard
    setBoard $ Map.delete position board

deletePieces :: [Position] -> STIO GameState ()
deletePieces [] = return ()
deletePieces (position:positions) = do
    deletePiece position
    deletePieces positions

getAllPieces :: STIO GameState [(Position, (Player, PieceType))]
getAllPieces = do
    board <- getBoard
    return $ Map.toList board

getAllPositions :: [Position]
getAllPositions = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
    