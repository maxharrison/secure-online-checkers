module Main where

-- Haskell modules
import Data.Char (chr)

-- My modules
import State
import GameState
import Input
import Utils
import Display
import Valid








playMove :: STIO GameState ()
playMove = do
    route <- lift $ getRoute
    validMove <- valid route
    if validMove
        then makeMove route
        else do lift $ putStrLn "Invalid move"
                playMove



-- TODO: Include the AI code from the earlier version
play :: STIO GameState ()
play = do
    result <- whosWon
    case result of
        Just player -> do
            displayBoard
            lift $ putStrLn $ "Player " ++ (show player) ++ " wins!"
        Nothing -> do -- if the game has not ended
            displayBoard
            playMove
            nextPlayer
            play



main :: IO ()
main = do
    (_, gameState) <- app play (GameState testBoard White 0)
    putStrLn "Game Finished"




