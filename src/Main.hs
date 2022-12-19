module Main where

import Types (Board, Player(..))
import Utils (whosWon, king, next)
import Utils (startBoard, test1Board, winingTest, kingTest__)
import Display (displayBoard)
import Input (getCoordinates)
import Valid (valid)
import Move (move)
import AI (playAI)

import AI_Tests


play :: Board -> Player -> IO()
play b p
    | whosWon b == Just B = do displayBoard b
                               putStrLn "\nPlayer B wins!\n"
    | whosWon b == Just W = do displayBoard b
                               putStrLn "\nPlayer W wins!\n"
    | otherwise = do putStrLn $ "\nPlayer " ++ show (king p) ++ ":"
                     displayBoard b
                     m <- getCoordinates
                     if not $ valid b p m
                         then do putStrLn "Invalid move"
                                 play b p
                         else let b' = move b m
                              in play b' (next p)


main :: IO()
main = do
    main2
{-     putStrLn "\nEnter 'A' to play against an AI, or enter 'B' to play a two player game.\n"
    input <- getLine
    case input of
        "A" -> playAI startBoard W
        "B" -> play startBoard W
        _   -> do putStrLn "\nInvalid input"
                  main -}

