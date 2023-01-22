module Valid where

-- Haskell modules
import Control.Monad (filterM)

-- My modules
import GameState
import Utils
import State

type GameStateIO a = STIO GameState a

{- valid :: Position -> Position -> GameStateIO Bool
valid origin destination = do
    player <- getCurrentPlayer
    originPiece <- getPiece origin
    destinationPiece <- getPiece destination
    let isOwnPiece = originPiece == Just (player, _)
    let destinationEmpty = destinationPiece == Nothing
    return $ isOwnPiece && destinationEmpty
 -}

{- validPawn :: Position -> Position -> GameStateIO Bool
validPawn board player (origin, destination) = do
    return True


validKing :: Position -> Position -> GameStateIO Bool
validKing board player (origin, destination) = do
    return True -}



{- validMoves :: Position -> GameStateIO [Position]
validMoves origin = do
    allPieces <- getAllPieces
    let allPositions = map fst allPieces
    return $ filter (valid origin) allPositions -}


-- is player moving their own piece?
isOnBoard :: Position -> Bool
isOnBoard (x, y) =
    x >= 0 && x < size && y >= 0 && y < size


-- is player moving their own piece?
isOwnPiece :: Position -> GameStateIO Bool
isOwnPiece position = do return True
    --currentPlayer <- getCurrentPlayer
    --piece <- getPiece position
    --case piece of
    --    Just (player, _) -> return $ player == currentPlayer
    --    Nothing          -> return False

-- is destination empty?
isDestinationEmpty :: Position -> GameStateIO Bool
isDestinationEmpty position = do
    piece <- getPiece position
    return $ piece == Nothing

-- is the move diagonal?
isDiagonal :: Position -> Position -> Bool
isDiagonal origin destination =
    abs (fst origin - fst destination) == abs (snd origin - snd destination)

-- is the piece moving correctly?
isMovingCorrectly :: Position -> Position -> GameStateIO Bool
isMovingCorrectly origin destination = do
    currentPlayer <- getCurrentPlayer
    isKingAnswer <- isKing origin
    case isKingAnswer of
         True  -> return True
         False -> case currentPlayer of
                       White -> return $ snd destination < snd origin
                       Black -> return $ snd destination > snd origin


-- is king?
isKing :: Position -> GameStateIO Bool
isKing position = do
    piece <- getPiece position
    case piece of
        Just (_, King) -> return True
        _              -> return False



-- is there a distance of one?
isDistanceOne :: Position -> Position -> Bool
isDistanceOne origin destination =
    abs (fst origin - fst destination) == 1 && abs (snd origin - snd destination) == 1


-- is there a distance of two?
isDistanceTwo :: Position -> Position -> Bool
isDistanceTwo origin destination =
    abs (fst origin - fst destination) == 2 && abs (snd origin - snd destination) == 2

-- Is the middle piece an enemy?
isMiddleEnemy :: Position -> Position -> GameStateIO Bool
isMiddleEnemy origin destination = do
    currentPlayer <- getCurrentPlayer
    let middle = ((fst origin + fst destination) `div` 2, (snd origin + snd destination) `div` 2)
    middlePiece <- getPiece middle
    case middlePiece of
        Just (player, _) -> return $ player /= currentPlayer
        Nothing          -> return False


validSingleNoJump :: Position -> Position -> GameStateIO [Position]
validSingleNoJump origin destination = do
    -- are the pieces on the board?
    let isOnBoardAnswer = isOnBoard origin && isOnBoard destination
    -- is player moving their own piece?
    isOwnPieceAnswer <- isOwnPiece origin
    -- is destination empty?
    isDestinationEmptyAnswer <- isDestinationEmpty destination
    -- is the move diagonal?
    let isDiagonalAnswer = isDiagonal origin destination
    -- is the piece moving correctly?
    isMovingCorrectlyAnswer <- isMovingCorrectly origin destination
    -- is there a distance of one?
    let isDistanceOneAnswer = isDistanceOne origin destination
    -- the answer
    let validSingleNoJumpAnswer = isOnBoardAnswer && isOwnPieceAnswer && isDestinationEmptyAnswer &&
                                  isDiagonalAnswer && isMovingCorrectlyAnswer && isDistanceOneAnswer
    
    if validSingleNoJumpAnswer then return (origin:destination:[]) else return []


validSingleJump :: Position -> Position -> GameStateIO [Position]
validSingleJump origin destination = do
    -- are the pieces on the board?
    let isOnBoardAnswer = isOnBoard origin && isOnBoard destination
    -- is player moving their own piece?
    isOwnPieceAnswer <- isOwnPiece origin
    -- is destination empty?
    isDestinationEmptyAnswer <- isDestinationEmpty destination
    -- is the move diagonal?
    let isDiagonalAnswer = isDiagonal origin destination
    -- is the piece moving correctly?
    isMovingCorrectlyAnswer <- isMovingCorrectly origin destination
    -- is there a distance of two?
    let isDistanceTwoAnswer = isDistanceTwo origin destination
    -- is the middle piece an enemy?
    isMiddleEnemyAnswer <- isMiddleEnemy origin destination
    -- the answer
    let validSingleJumpAnswer = isOnBoardAnswer && isOwnPieceAnswer && isDestinationEmptyAnswer &&
                                isDiagonalAnswer && isMovingCorrectlyAnswer && isDistanceTwoAnswer && isMiddleEnemyAnswer
    
    if validSingleJumpAnswer then return (origin:destination:[]) else return []





{- validSinglePositionsFromOrigin :: Position -> GameStateIO [Position]
validSinglePositionsFromOrigin origin = do
    let allPositions = getAllPositions
    -- filter out all invalid positions
    validPositions <- filterM (validSingleNoJump origin) allPositions
    -- if there exists a valid jump, then only return the valid jumps
    let validJumps = filter snd validPositions
    if length validJumps > 0
        then return $ map fst validJumps
        else return $ map fst validPositions
 -}










{- 

validMove :: Move -> GameStateIO Bool
validMove (origin, jumps, destination) = do return True
    {- if length jumps == 0
        then validSingleNoJump origin destination
        else validMulti origin jumps destination -}
    

-}

get_routes :: Int -> Position -> [Position]
get_routes n (x, y) = 
    [
        (x+n, y+n),
        (x+n, y-n),
        (x-n, y+n),
        (x-n, y-n)
    ]





 

valid_routes :: Position -> GameStateIO [[Position]]
valid_routes origin = do
    single_routes <- mapM (validSingleNoJump origin) (get_routes 1 origin)
    jump_routes <- mapM (validSingleJump origin) (get_routes 2 origin)
    return $ single_routes ++ jump_routes

 




{- valid_routes3 :: [Position] -> GameStateIO [[Position]]
valid_routes3 (origin:route) = do
    
    single_routes <- mapM validSingleNoJump (get_routes 1 origin)
    jump_routes <- mapM validSingleJump (get_routes 2 origin)

    let hasJumped = length route > 0 -- might be wrong
    let hasSingle = length single_routes > 0
    let hasJump = length jump_routes > 0

    case (hasJumped, hasSingle, hasJump) of 
         (False    , _        , False  ) -> -- no jumps to do, first check. Return the single routes
         (False    , _        , True   ) -> -- first check, but has jumps
         (True     , _        , False  ) -> -- has jumped, but no jumps avialible, so return this route
         _                               ->  
    
    single_routes <- mapM validSingleNoJump (get_routes 1 origin)
    jump_routes <- mapM validSingleJump (get_routes 2 origin)
     
    if (length route == 0)

        -- if there are no jumps, then return the valid single moves
        then do if length jump_routes /= 0
                    then return jump_routes
                    else return single_routes

        -- if there are jumps, then return the valid multi moves
        else do lift $ putStrLn $ "ROUTE IS LONGER" 
                return jump_routes
 -}






 



{- -- origin is just the first position in the route
--             route                     routes
valid_moves :: [Position] -> GameStateIO [[Position]]
valid_moves (x, y) route = do
    lift $ putStrLn $ "valid_moves: " ++ show (x, y)





    single_moves_valid <- filterM (validSingleNoJump (x, y)) [(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]

    jumps_valid <- filterM (validSingleJump (x, y)) [(x+2, y+2), (x+2, y-2), (x-2, y+2), (x-2, y-2)]

    additional_jumps <- concat <$> mapM (valid_moves) jumps_valid

    case (length jumps_valid, length additional_jumps) of
        (0, _) -> do lift $ putStrLn $ "valid_moves smv: " ++ show (x, y) ++ " . " ++ "  SMV " ++ show single_moves_valid  ++ "  JV " ++ show jumps_valid ++ "  AJV " ++ show additional_jumps
                     return single_moves_valid
        (_, 0) -> do lift $ putStrLn $ "valid_moves jv : " ++ show (x, y) ++ " . " ++ "  SMV " ++ show single_moves_valid  ++ "  JV " ++ show jumps_valid ++ "  AJV " ++ show additional_jumps
                     return jumps_valid
        (_, _) -> do lift $ putStrLn $ "valid_moves ajv: " ++ show (x, y) ++ " . " ++ "  SMV " ++ show single_moves_valid  ++ "  JV " ++ show jumps_valid ++ "  AJV " ++ show additional_jumps
                     return additional_jumps  -}



--canMultipleJump :: Board -> Player -> Move -> Bool
--canMultipleJump board player (origin, destination) =
--    let possibleJumps = getPossibleJumps ----
--    in isReachable destination jumps

{- 


-- HELP FROM https://stackoverflow.com/questions/37511246/haskell-checkers-how-to-write-a-function-that-returns-a-list-of-possible-jumps
singleJumps :: Player -> Coordinate -> Board -> [(Coordinate, Board)]
singleJumps p (x, y) b = [(destination, b') | destination <- [(x+2, y+2), (x+2, y-2), (x-2, y+2), (x-2, y-2)], 
                                        isPieceBetweenOtherPlayer b p ((x, y), destination),
                                        b' <- [jump b ((x, y), destination)]]

-- HELP FROM https://stackoverflow.com/questions/37511246/haskell-checkers-how-to-write-a-function-that-returns-a-list-of-possible-jumps
multiJumps :: Player -> Coordinate -> Board -> [([Coordinate], Board)]
{- 
for each possible single jump (rc, b):
  for each possible multi jump (path, b') starting from (rc,b):
    return the path (rc:path) and ending board configuration b'
-}
multiJumps p origin b = [([dst] ++ dsts, b'') | (dst, b')   <- singleJumps p origin b,
                                                (dsts, b'') <- multiJumps p dst    b']

 -}