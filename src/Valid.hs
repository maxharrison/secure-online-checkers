module Valid where

-- Haskell modules
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace


-- My modules
import GameState




--------------------------------------------------------------------------------
--                           Validation Function                              --
--------------------------------------------------------------------------------

single x = [x]


-- Returns the longest route
longest :: [[a]] -> [[a]]
longest xs = let max = maximum $ map length xs
             in filter (\x -> length x == max) xs



getAllValidBoardsRoutes :: Board -> Player -> [(Board, Route)]
getAllValidBoardsRoutes board player
    | won board /= Nothing = []
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
    

generateValidRoute :: Board -> Player -> Route -> [(Route)]
generateValidRoute board player route =
    let routes = [route ++ [destination] | destination <- getAllPositions]
        validRoutes = mapMaybe (\r ->
            case validRoute True board player r of
                Just board' -> Just (r)
                Nothing -> Nothing) routes
        


            --case validRoute True board player r of
            --    Just board' -> Just (r)
            --    Nothing -> Nothing) routes

    in if length validRoutes > 0
        then concatMap (generateValidRoute board player) validRoutes
        else if length route > 1
                then [route]
                else []






type IsFirstMove = Bool

validRoute :: IsFirstMove -> Board -> Player -> Route -> Maybe (Board)
validRoute isFirstMove board player (origin:destination:route) =
    let isDistance2 = diagonalDistance origin destination == 2
        middle = getMiddle origin destination
        validBasic = validOwnPiece board player origin
                  && validInBounds origin destination
                  && validPositionEmpty board destination
                  && validMovingCorrectly board origin destination
                  && validCorrectDistance isFirstMove origin destination
                  && validJump isDistance2 board player middle
        newBoard = let Just originPiece = Map.lookup origin board
                       board' = (Map.delete origin . Map.insert destination originPiece) board
                   in if isDistance2 then Map.delete middle board' else board'
        
    in case (isDistance2, validBasic) of
            (_, False) -> Nothing -- NOT VALID
            (False, _) -> if null route
                                then return newBoard
                                else Nothing
            (True, _)  -> if null route
                                then return newBoard
                                else validRoute False newBoard player (destination:route)


validRoute2 :: IsFirstMove -> Board -> Player -> Route -> Maybe (Board)
validRoute2 isFirstMove board player (origin:destination:route) =
    let isDistance2 = diagonalDistance origin destination == 2
        middle = getMiddle origin destination
    in if (validOwnPiece board player origin
        && validInBounds origin destination
        && validPositionEmpty board destination
        && validMovingCorrectly board origin destination
        && validCorrectDistance isFirstMove origin destination
         && validJump isDistance2 board player middle)
            then let Just originPiece = Map.lookup origin board
                     board' = (Map.delete origin . Map.insert destination originPiece) board
                     board'' = if isDistance2 then Map.delete middle board' else board'
                 in if null route
                        then return board''
                        else validRoute2 False board'' player (destination:route)
            else Nothing


--------------------------------------------------------------------------------
--                        Validation Helper Functions                         --
--------------------------------------------------------------------------------


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


--------------------------------------------------------------------------------
--                          General Helper Functions                          --
--------------------------------------------------------------------------------
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

won :: Board -> Maybe Player
won board =
    let whitePieces = countPlayerPieces board White
        blackPieces = countPlayerPieces board Black
    in case (whitePieces, blackPieces) of
        (0, _) -> Just Black
        (_, 0) -> Just White
        _      -> Nothing

countPlayerPieces :: Board -> Player -> Int
countPlayerPieces board player =
    length $ (filter (\(_, (p, _)) -> p == player)) $ Map.toList board