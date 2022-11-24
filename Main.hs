module Main where

import Data.Char (isDigit, isLetter, digitToInt, toUpper, ord, chr)
import ReadMove (coordinate)
import Display
import System.IO (hFlush, stdout)
import Types
import Move
import Win



startBoard :: Board
startBoard = [[Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False)],
              [Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        ],
              [Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False)],
              [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
              [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
              [Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        ],
              [Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False)],
              [Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        ]]


test :: Board
test = [[Nothing        , Nothing        , Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False)],
        [Just (B, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        ],
        [Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False), Nothing        , Just (W, False)],
        [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
        [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
        [Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        ],
        [Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False)],
        [Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        , Just (B, False), Nothing        ]]

test2 :: Board
test2 = [[Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Just (W, False), Nothing        , Just (W, False), Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Just (B, False), Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ],
         [Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        , Nothing        ]]




prompt :: String -> IO String
prompt s = do
   putStr $ s ++ ": "
   hFlush stdout
   getLine


next :: Player -> Player
next W = B 
next B = W

getCoordinates :: IO (Coordinate, Coordinate)
getCoordinates = do
    input1 <- prompt "\nPlease enter the coordinates of the piece you would like to move"
    case coordinate input1 of
        Nothing -> do
            putStrLn "Invalid input"
            (c1, c2) <- getCoordinates
            return (c1, c2)
        Just c1 -> do
            input2 <- prompt "Please enter the destination coordinates"
            case coordinate input2 of
                Nothing -> do
                    putStrLn "Invalid input"
                    (c1, c2) <- getCoordinates
                    return (c1, c2)
                Just c2 -> do
                    return (c1, c2)

play :: Board -> Player -> IO()
play board player = do
    showBoard board player
    (c1, c2) <- getCoordinates
    let piece = getPiece board c1
    let move = (piece, c1, c2)
    case movePiece board move of
        Nothing -> do 
            putStrLn "Invalid move"
            play board player
        Just (newBoard) -> do
            case whosWon newBoard of
                Nothing -> do
                    play newBoard (next player)
                Just winner -> do
                    putStr "\n"
                    showBoard newBoard player
                    putStr "\n"
                    putStrLn $ "Player " ++ (show winner) ++ " wins!"

main :: IO()
main = play test2 B