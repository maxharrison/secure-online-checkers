{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Control.Monad.IO.Class
import Data.IORef
import Data.Text.Lazy (pack)


import GameState
import qualified Data.Map as Map



-- Thomas M. DuBuisson - https://stackoverflow.com/questions/53024969/keep-state-between-http-request

port = 8080

main = server

type State = Int

server :: STIO State ()
server = do
    print ("Starting Server at port " ++ show port)
    lift $ scotty port $ do
        get "/start" $ do
            updateCounterByOneInState
            counter <- getCounterFromState
            json $ "{count: " ++ counter ++ "}"




scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    ref <- newState initialState
    scotty port (routes ref)

routes :: MyStateRef -> ScottyM()
routes ref = do
    get "/start/:id" (start ref)
    get "/poll/:id" (poll ref)


start :: MyStateRef -> ActionM ()
start ref = do
    id <- param "id"
    gameState <- getState ref
    case (whiteID gameState, blackID gameState) of
         ("", "") -> do setID id White ref
                        json ("{player: "++ "both blank" ++ id ++"}")
         (_, "")  -> do setID id Black ref
                        json ("{player: "++ "black is blank" ++ id ++"}")
         ("", _)  -> do setID id White ref
                        json ("{player: "++ "white is blank" ++ id ++"}")
         _        -> json ("{player: "++ "NONE" ++ id ++"}")



poll :: MyStateRef -> ActionM ()
poll ref = do
    id <- param "id"
    gameState <- getState ref
    case (whiteID gameState == id, blackID gameState == id) of
         (True, False) -> json ("{player: "++ "WHITE" ++"}")
         (False, True) -> json ("{player: "++ "BLACK" ++"}")
         _             -> json ("{player: "++ "NONE" ++"}")
    --json ("{player: "++ "WHITE" ++ id ++"}")


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
initialState = (GameState "" "" startBoard White 0)

type MyState = GameState
type MyStateRef = IORef GameState -- Could be TVar, MVar, DB address, etc

newState :: MonadIO m => MyState -> m MyStateRef
newState = liftIO . newIORef

getState :: MonadIO m => MyStateRef -> m MyState
getState ref = do
    gameState <- liftIO (readIORef ref)
    return gameState


setID :: MonadIO m => ID -> Player -> MyStateRef -> m MyState
setID id player ref = do
    gameState <- liftIO (readIORef ref)
    let newGameState = gameState {whiteID = id}
    newGameState `seq` liftIO (writeIORef ref newGameState)
    return newGameState
    --case player of
    --    White ->  `seq` liftIO (writeIORef ref y)
    --        return $ gameState {whiteID = id}
    --    Black -> return $ gameState {blackID = id}