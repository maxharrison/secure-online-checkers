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
import AI



playMove :: STIO GameState () -> STIO GameState ()
playMove f = do
    route <- lift $ getRoute
    board <- getBoard
    currentPlayer <- getCurrentPlayer
    let routes = getAllValidBoardsRoutes board currentPlayer
    if route `elem` (map snd routes)
        then do setBoard $ fst $ head $ filter (\(_, r) -> r == route) routes
                nextPlayer
                f
        else do lift $ putStrLn "Invalid move"
                playMove f



play2Player :: STIO GameState ()
play2Player = do
    result <- whosWon
    case result of
        Just player -> do
            displayBoard
            lift $ putStrLn $ "Player " ++ (show player) ++ " wins!"
        Nothing -> do -- if the game has not ended
            displayBoard
            playMove play2Player
            nextPlayer
            play2Player



playAI :: STIO GameState ()
playAI = do
    result <- whosWon
    case result of
        Just player -> do
            displayBoard
            lift $ putStrLn $ "Player " ++ (show player) ++ " wins!"
        Nothing -> do
            currentPlayer <- getCurrentPlayer
            case currentPlayer of
                White -> do
                    lift $ putStrLn "WHITEE"
                    displayBoard
                    playMove playAI
                    nextPlayer
                    playAI
                Black -> do
                    lift $ putStrLn "BLACK"
                    board <- getBoard

                    case bestMove 3 score_count board currentPlayer of
                        Just (newBoard, newPlayer, route) -> do
                            lift $ putStrLn ""
                            lift $ putStrLn $ show currentPlayer ++ " : " ++ show route
                            setBoard newBoard
                            nextPlayer
                            playAI
                        Nothing -> do
                            lift $ putStrLn "No valid moves"






main :: IO ()
main = do
    putStrLn "Welcome to Checkers!"
    putStrLn "Would you like to play against a friend or the computer?"
    putStrLn "1. Friend"
    putStrLn "2. Computer"
    choice <- getLine
    case choice of
            "1" -> do app play2Player (GameState startBoard White 0)
                      return ()
            "2" -> do putStrLn "AI"
                      app playAI (GameState startBoard White 0)
                      return ()
            _   -> do main
                      return ()



