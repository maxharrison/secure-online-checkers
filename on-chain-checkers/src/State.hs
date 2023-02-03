module State where

{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Data.IORef
import Data.Text.Lazy (pack)

import qualified Data.Map as Map
import GameState

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

startBoard :: Board
startBoard = Map.fromList([
    ((1, 7), (White, Pawn)), ((3, 7), (White, Pawn)), ((5, 7), (White, Pawn)), ((7, 7), (White, Pawn)),
    ((0, 6), (White, Pawn)), ((2, 6), (White, Pawn)), ((4, 6), (White, Pawn)), ((6, 6), (White, Pawn)),
    ((1, 5), (White, Pawn)), ((3, 5), (White, Pawn)), ((5, 5), (White, Pawn)), ((7, 5), (White, Pawn)),
    ((0, 2), (Black, Pawn)), ((2, 2), (Black, Pawn)), ((4, 2), (Black, Pawn)), ((6, 2), (Black, Pawn)),
    ((1, 1), (Black, Pawn)), ((3, 1), (Black, Pawn)), ((5, 1), (Black, Pawn)), ((7, 1), (Black, Pawn)),
    ((0, 0), (Black, Pawn)), ((2, 0), (Black, Pawn)), ((4, 0), (Black, Pawn)), ((6, 0), (Black, Pawn))
    ])

    
initialState :: GameState
initialState = (GameState startBoard White 0)

type MyState = GameState
type MyStateRef = IORef GameState -- Could be TVar, MVar, DB address, etc

newState :: MonadIO m => MyState -> m MyStateRef
newState = liftIO . newIORef

getState :: MonadIO m => MyStateRef -> m MyState
getState ref = do
    gameState <- liftIO (readIORef ref)
    return gameState

{- nextPlayer :: MonadIO m => MyStateRef -> m MyState
statefulStuff ref =
 do x <- liftIO (readIORef ref)
    -- N.B. lack of atomicity - that isn't the point of this answer
    let y = x
    y `seq` liftIO (writeIORef ref y)
    pure y -}