module Valid where

-- Haskell modules
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace


-- My modules
import GameState


----------------------------------------------------------------
--                    Validation Function                     --
----------------------------------------------------------------

single x = [x]

-- From the routes, only return the ones that they are allowed to take
-- For example, if a jump can be taken, they have to take a jump
longest :: [Route] -> [Route]
longest rs = let l = maximum $ map routeLength rs
             in filter (\r -> routeLength r == l) rs

routeLength :: Route -> Int
routeLength r =
  case length r of
    0 -> 0
    1 -> 0
    2 -> if (diagonalDistance (r!!0) (r!!1) == 2)
         then 2
         else 1
    l -> l

getAllValidBoardsRoutes :: Board -> Player -> [(Board, Route)]
getAllValidBoardsRoutes board player
    | countAllPieces board == 0 = []
    | otherwise = map (\r ->
        case validRoute True board player r of
            Just board' -> (board', r)
            Nothing -> error "This should never happen"
        ) $ validRoutes board player

validRoutes :: Board -> Player -> [(Route)]
validRoutes board player =
    let originRoutes = map single $
            (Map.keys . Map.filter (\(p, _) -> p == player)) board
        routes = concatMap (generateValidRoute board player) originRoutes
    in longest routes
    

-- The 'generateValidRoute' function generates all valid routes for a
-- given player and an initial route on the board.
generateValidRoute :: Board -> Player -> Route -> [(Route)]
generateValidRoute board player route =
      -- Generate a list of potential routes by appending each position from 'getAllPositions' to the initial route
    let routes = [route ++ [destination] | destination <- getAllPositions]

        -- Filter out the invalid routes using the 'validRoute' function
        validRoutes = mapMaybe (\r ->
            case validRoute True board player r of
                -- If the route is valid, return the route wrapped in a 'Just' value
                Just board' -> Just (r)
                -- If the route is not valid, return 'Nothing'
                Nothing -> Nothing) routes

    -- If there are any valid routes, recursively call the function for
    -- each valid route and concatenate the results
    in if length validRoutes > 0
        then concatMap (generateValidRoute board player) validRoutes
        -- If the length of the initial route is greater than 1 and there are no valid routes
        -- to extend it, return a list containing the initial route
        else if length route > 1
                then [route]
                -- If the length of the initial route is not greater than 1 and there are no valid
                -- routes to extend it, return an empty list
                else []


type IsFirstMove = Bool

-- The 'validRoute' function checks if a given route is valid for a player to move their piece
-- on the board. If the move is valid, it returns the updated board after making the move;
-- otherwise, it returns Nothing.
validRoute :: IsFirstMove -- ^ A 'Bool' indicating whether it's the first move in a game.
           -> Board       -- ^ The board state.
           -> Player      -- ^ The current player.
           -> Route       -- ^ A list of coordinates.
           -> Maybe (Board)
validRoute isFirstMove board player (origin:destination:route) =

    -- Check if the distance between the origin and destination is 2 (i.e., a jump move)
    let isDistance2 = diagonalDistance origin destination == 2

        -- Calculate the middle position between the origin and destination
        middle = getMiddle origin destination

        -- Check if the basic conditions for a valid move are satisfied:
        -- 1. The piece at the origin belongs to the current player
        -- 2. The origin and destination are within the board bounds
        -- 3. The destination position is empty
        -- 4. The piece is moving in the correct direction
        -- 5. The piece is moving the correct distance for its first move
        -- 6. If it's a jump move, the piece in the middle belongs to the opponent
        validBasic = validOwnPiece board player origin
                  && validInBounds origin destination
                  && validPositionEmpty board destination
                  && validMovingCorrectly board origin destination
                  && validCorrectDistance isFirstMove origin destination
                  && validJump isDistance2 board player middle

        -- Calculate the new board state after making the move
        newBoard = makeMove board player origin destination middle isDistance2
        
    in case (isDistance2, validBasic) of

            -- If the basic conditions are not met, the move is invalid
            (_, False) -> Nothing
            
            -- If it's not a jump move and the route is empty, the move is valid
            (False, _) -> if null route
                            then return newBoard
                            else Nothing

            -- If it's a jump move and the route is empty, the move is valid;
            -- otherwise, continue checking the next move in the route
            (True, _)  -> if null route
                            then return newBoard
                            else validRoute False newBoard player (destination:route)


----------------------------------------------------------------
--                Validation Helper Functions                 --
----------------------------------------------------------------

validInBounds :: Position -> Position -> Bool
validInBounds (xo, yo) (xd, yd) =
    xo >= 0 && xo < size && yo >= 0 && yo < size &&
    xd >= 0 && xd < size && yd >= 0 && yd < size

-- checks if the player is moving their own piece
validOwnPiece :: Board -> Player -> Position -> Bool
validOwnPiece board player position =
    case Map.lookup position board of
        Just (p, _) -> p == player
        _           -> False

-- checks if the position is empty
validPositionEmpty :: Board -> Position -> Bool
validPositionEmpty board position =
    Map.lookup position board == Nothing

-- if this is the first move in the route then it can make
-- single distance moves
validCorrectDistance :: IsFirstMove -> Position -> Position -> Bool
validCorrectDistance isFirstMove origin destination
    | isFirstMove = (diagonalDistance origin destination == 1 ||
                     diagonalDistance origin destination == 2)
    | otherwise = diagonalDistance origin destination == 2

-- if the user is jumping then is the position in the
-- middle an oppponent piece
validJump :: Bool -> Board -> Player -> Position -> Bool
validJump isDistance2 board player middle
    | isDistance2 = case Map.lookup middle board of
        Just (p, _) -> p == next player
        _           -> False
    | otherwise = True

validMovingCorrectly :: Board -> Position -> Position -> Bool
validMovingCorrectly board (xo, yo) (xd, yd) =
    let xDifference = abs (xo - xd)
        yDifference = abs (yo - yd)
    in if xDifference == yDifference
        then case Map.lookup (xo, yo) board of
            Just (White, Pawn) -> yd < yo
            Just (White, King) -> True
            Just (Black, Pawn) -> yd > yo
            Just (Black, King) -> True
            Nothing            -> False
        else False


----------------------------------------------------------------
--                  General Helper Functions                  --
----------------------------------------------------------------

makeMove :: Board -> Player -> Position -> Position -> Position -> Bool -> Board
makeMove board player origin destination middle isDistance2 =
    let originPiece = fromJust $ Map.lookup origin board
        piece = if (player == White && snd destination == 0) ||
                   (player == Black && snd destination == 7)
                then (player, King)
                else originPiece
        board' = (Map.delete origin . Map.insert destination piece) board
    in if isDistance2 then Map.delete middle board' else board'

diagonalDistance :: Position -> Position -> Int
diagonalDistance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

getMiddle :: Position -> Position -> Position
getMiddle (yOrigin, xOrigin) (yDestination, xDestination) =
    let yMiddle = (yOrigin + yDestination) `div` 2
        xMiddle = (xOrigin + xDestination) `div` 2
    in (yMiddle, xMiddle)

next :: Player -> Player
next White = Black
next Black = White

countAllPieces :: Board -> Int
countAllPieces board =
    let whitePieces = countPlayerPieces board White
        blackPieces = countPlayerPieces board Black
    in whitePieces + blackPieces

countPlayerPieces :: Board -> Player -> Int
countPlayerPieces board player =
    length $ (filter (\(_, (p, _)) -> p == player)) $ Map.toList board

getAllPositions :: [Position]
getAllPositions = [(x, y) | x <- [0..size-1], y <- [0..size-1]]