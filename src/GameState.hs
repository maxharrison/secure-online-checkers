module GameState where

-- Haskell modules
import qualified Data.Map as Map

----------------------------------------------------------------
--                           Types                            --
----------------------------------------------------------------

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
  started       :: Bool,
  whiteID       :: Maybe String,
  whiteKey      :: Word,
  blackID       :: Maybe String,
  blackKey      :: Word,
  ai            :: Bool
}

type Route = [Position]


----------------------------------------------------------------
--                             Misc                           --
----------------------------------------------------------------


edgeCaseBoards :: [(String, Board)]
edgeCaseBoards = [
    ("Test 1",
      Map.fromList [
        ((1, 5), (White, Pawn)),
        ((0, 2), (Black, Pawn))
      ]
    ),

    ("Test 2",
      Map.fromList [
        ((1, 5), (White, Pawn)),
        ((1, 3), (Black, Pawn)),
        ((2, 2), (Black, Pawn))
      ]
    ),

    ("Test 3",
      Map.fromList [
        ((1, 5), (White, Pawn)),
        ((2, 4), (Black, Pawn)),
        ((0, 2), (Black, Pawn))
      ]
    ),

    ("Test 4",
      Map.fromList [
        ((1, 7), (White, Pawn)),
        ((2, 6), (White, Pawn)),
        ((0, 4), (Black, Pawn))
      ]
    ),

    ("Test 5",
      Map.fromList [
        ((1, 1), (White, King)),
        ((0, 2), (Black, Pawn))
      ]
    ),

    ("Test 6",
      Map.fromList [
        ((1, 7), (White, Pawn)),
        ((0, 4), (Black, King))
      ]
    ),

    ("Test 7",
      Map.fromList [
        ((1, 3), (White, King)),
        ((2, 4), (Black, Pawn)),
        ((0, 2), (Black, Pawn))
      ]
    ),

    ("Test 8",
      Map.fromList [
        ((1, 7), (White, Pawn)),
        ((2, 6), (White, Pawn)),
        ((0, 4), (Black, King))
      ]
    ),

    ("Test 9",
      Map.fromList [
        ((1, 7), (White, Pawn)),
        ((0, 6), (White, Pawn)),
        ((1, 5), (White, Pawn)),
        ((0, 4), (Black, Pawn)),
        ((2, 4), (Black, Pawn))
      ]
    ),

    ("Test 10",
      Map.fromList [
        ((1, 7), (White, Pawn)),
        ((0, 6), (White, Pawn)),
        ((1, 5), (White, Pawn)),
        ((3, 5), (White, Pawn)),
        ((0, 2), (Black, Pawn)),
        ((2, 2), (Black, Pawn))
      ]
    )
  ]


testBoard :: Board
testBoard = Map.fromList([((1, 1), (White, Pawn)),((2, 5), (White, King)), ((3, 4), (Black, Pawn)), ((5, 2), (Black, Pawn))])


smallBoard :: Board
smallBoard = Map.fromList([
    ((1, 5), (White, Pawn)), ((3, 5), (White, Pawn)), ((5, 5), (White, Pawn)), ((7, 5), (White, Pawn)),
    ((0, 2), (Black, Pawn)), ((2, 2), (Black, Pawn)), ((4, 2), (Black, Pawn)), ((6, 2), (Black, Pawn))
  ])


startBoard :: Board
startBoard = Map.fromList([
    ((1, 7), (White, Pawn)), ((3, 7), (White, Pawn)), ((5, 7), (White, Pawn)), ((7, 7), (White, Pawn)),
    ((0, 6), (White, Pawn)), ((2, 6), (White, Pawn)), ((4, 6), (White, Pawn)), ((6, 6), (White, Pawn)),
    ((1, 5), (White, Pawn)), ((3, 5), (White, Pawn)), ((5, 5), (White, Pawn)), ((7, 5), (White, Pawn)),
    ((0, 2), (Black, Pawn)), ((2, 2), (Black, Pawn)), ((4, 2), (Black, Pawn)), ((6, 2), (Black, Pawn)),
    ((1, 1), (Black, Pawn)), ((3, 1), (Black, Pawn)), ((5, 1), (Black, Pawn)), ((7, 1), (Black, Pawn)),
    ((0, 0), (Black, Pawn)), ((2, 0), (Black, Pawn)), ((4, 0), (Black, Pawn)), ((6, 0), (Black, Pawn))
  ])


showPosition :: Board -> Position -> String
showPosition board position =
    case Map.lookup position board of
        Just (White, Pawn) -> "w"
        Just (White, King) -> "W"
        Just (Black, Pawn) -> "b"
        Just (Black, King) -> "B"
        Nothing            -> "-"


showRow :: Board -> Int -> String
showRow board y =
    concat [showPosition board (x, y) ++ " " | x <- [0..(size-1)]]


showBoard :: Board -> String
showBoard board = concat $ reverse [showRow board y ++ "\n" | y <- [0..(size-1)]]


showFullBoard :: Board -> Player -> String
showFullBoard board player = 
    let rows = lines $ showBoard board
        numbers = reverse [n | n <- [0..size]]
        letters = [c | c <- ['A'..toEnum (64 + size)]]
        string = "\n" ++ concat [show n ++ " " ++ r ++ "\n" | (n, r) <- zip numbers rows]
                                 ++ "  " ++ concat [[l, ' '] | l <- letters]
    in ("Current player: " ++ (show player) ++ "\n" ++ string ++ "\n")