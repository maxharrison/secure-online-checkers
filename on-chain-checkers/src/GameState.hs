module GameState where

-- Haskell modules
import qualified Data.Map as Map



size :: Int
size = 8

data Player = White | Black
    deriving (Eq, Show)

data PieceType = Pawn | King
    deriving (Eq, Show)

type Position = (Int, Int)

type Board = Map.Map Position (Player, PieceType)

type ID = String

data GameState = GameState {
    whiteID       :: ID,
    blackID       :: ID,
    board         :: Board,  
    currentPlayer :: Player,   
    moveNumber    :: Integer
    }
