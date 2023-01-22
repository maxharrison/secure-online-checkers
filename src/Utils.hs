module Utils where

-- Haskell modules

-- My modules
import GameState
import State




getCaptured :: [Position] -> [Position]
getCaptured [] = []
getCaptured [_] = []
getCaptured (p1:p2:positions) = 
    if isDistance p1 p2 2
        then (getMiddlePosition p1 p2) : getCaptured (p2:positions)
        else getCaptured (p2:positions)


isDistance :: Position -> Position -> Int -> Bool
isDistance (x1, y1) (x2, y2) n =
    abs(x2-x1) == n && abs(y2-y1) == n


getMiddlePosition :: Position -> Position -> Position
getMiddlePosition (x1, y1) (x2, y2) =
    (x1 + (x2-x1) `div` 2, y1 + (y2-y1) `div` 2)



-- only checks whether the origin is not empty
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
        Nothing -> return ()


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