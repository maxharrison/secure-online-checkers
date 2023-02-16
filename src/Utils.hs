module Utils where

-- Haskell modules

-- My modules
import GameState
import State





{- -- only checks whether the origin is not empty
makeMove :: Route -> STIO GameState ()
makeMove route = do
    let origin = head route
    let destination = last route
    originPiece <- getPiece origin
    case originPiece of
        Just (player, pieceType) -> do
            insertPiece destination (player, pieceType)
            --deletePiece origin
            let captured = getCaptured route
            deletePieces (origin:captured)
            -- get pieces between all the jumps and delete them
        Nothing -> return () -}


countPlayerPieces :: Player -> STIO GameState Int
countPlayerPieces player = do
    allPieces <- getAllPieces
    return $ length $ (filter (\(_, (p, _)) -> p == player)) allPieces


whosWon :: STIO GameState (Maybe Player)
whosWon = do
    -- TODO: also should check whether a player can make any moves
    whitePieces <- countPlayerPieces White
    blackPieces <- countPlayerPieces Black
    case (whitePieces, blackPieces) of
        (0, 0) -> return Nothing
        (0, _) -> return $ Just Black
        (_, 0) -> return $ Just White
        _      -> return Nothing

nextPlayer :: STIO GameState ()
nextPlayer = do
    player <- getCurrentPlayer
    case player of
        White -> setCurrentPlayer Black
        Black -> setCurrentPlayer White