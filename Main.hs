module Main where

import Types (Board, Player(..))
import Utils (whosWon, startBoard, king, next, test1Board)
import Display (displayBoard)
import Input (getCoordinates)
import Valid (valid)
import Move (move)







play :: Board -> Player -> IO()
play b p
    | whosWon b == Just B = do displayBoard b
                               putStrLn "\nPlayer B wins!\n"
    | whosWon b == Just W = do displayBoard b
                               putStrLn "\nPlayer W wins!\n"
    | False = putStrLn "Draw!" -- TODO: Implement this
    | otherwise = do putStrLn $ "Player " ++ show (king p) ++ ":"
                     displayBoard b
                     m <- getCoordinates
                     if not $ valid b p m
                         then do putStrLn "Invalid move"
                                 play b p
                         else let b' = move b m
                              in play b' (next p)



{- pla1 :: Board -> Player -> IO()
pla1 board player = do
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
                    putStrLn $ "Player " ++ (show winner) ++ " wins!" -}


{- pla2 :: Board -> Player -> IO()
pla2 b p | wins O b = putStrLn "Player O wins"
         | wins X b = putStrLn "Player X wins"
         | full b = putStrLn "It is a draw"
         | p == O = do
              showBoard b
              col <- getMove O
              play (move O col b) X
         | p == X = do
              showBoard b
              --col <- getMove X
              --play (move X col b) O
              putStrLn "Player X is thinking...\n"
              play (bestMove b p) O
         | otherwise = putStrLn "Error" -}


main :: IO()
main = play startBoard B