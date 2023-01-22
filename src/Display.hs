module Display where

-- Haskell modules
import qualified Data.Map as Map

-- My modules
import GameState
import State



testBoard :: Board
testBoard = Map.fromList([((2, 5), (White, King)), ((3, 4), (Black, Pawn)), ((5, 2), (Black, Pawn))])

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
        Just (White, Pawn) -> "♦"
        Just (White, King) -> "W"
        Just (Black, Pawn) -> "♢"
        Just (Black, King) -> "B"
        Nothing            -> "-"

showRow :: Board -> Int -> String
showRow board y =
    concat [showPosition board (x, y) ++ " " | x <- [0..(size-1)]]

showBoard :: Board -> String
showBoard board = concat $ reverse [showRow board y ++ "\n" | y <- [0..(size-1)]]

displayBoard :: STIO GameState ()
displayBoard = do
    currentPlayer <- getCurrentPlayer
    lift $ putStrLn $ "\nCurrent player: " ++ show currentPlayer
    board <- getBoard
    let rows = lines $ showBoard board
    let numbers = reverse [n | n <- [0..size]]
    let letters = [c | c <- ['A'..toEnum (64 + size)]]
    let string = "\n" ++ concat [show n ++ " " ++ r ++ "\n" | (n, r) <- zip numbers rows]
                                 ++ "  " ++ concat [[l, ' '] | l <- letters]
    lift $ putStrLn string


