module Valid where

-- Haskell modules
import qualified Data.Map as Map
import Debug.Trace

-- My modules
import GameState
import Utils
import State



--------------------------------------------------------------------------------
--                           Validation Functions                             --
--------------------------------------------------------------------------------

valid :: Route -> STIO GameState Bool
valid route = do
    board <- getBoard
    currentPlayer <- getCurrentPlayer
    return $ validRoute True board currentPlayer route

-- is this the first move in the route
type IsFirstMove = Bool

validRoute :: IsFirstMove -> Board -> Player -> Route -> Bool
validRoute isFirstMove board player route =
    let (origin:destination:remainingRoute) = route
        isDistance2 = diagonalDistance origin destination == 2

    in validOwnPiece board player origin

    && validPositionEmpty board destination

    && validMovingCorrectly board origin destination

    && validCorrectDistance isFirstMove origin destination

    && validJump isDistance2 board player origin destination


    && (if null remainingRoute
            then True
            else let Just originPiece = Map.lookup origin board
                     board2 = deleteBoard board origin
                     board3 = insertBoard board2 destination originPiece
                     board4 = if isDistance2
                        then deleteBoard board3 (getMiddle origin destination)
                        else board3
                 in validRoute False board4 player (destination:remainingRoute))





--------------------------------------------------------------------------------
--                        Validation Helper Functions                         --
--------------------------------------------------------------------------------


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
validCorrectDistance isFirstMove origin destination =
    if isFirstMove
        then (diagonalDistance origin destination == 1 ||
              diagonalDistance origin destination == 2)
        else diagonalDistance origin destination == 2

-- if the user is jumping then is the position in the
-- middle an oppponent piece
validJump :: Bool -> Board -> Player -> Position -> Position -> Bool
validJump isDistance2 board player origin destination =
    if not isDistance2
        then True
        else let middle = getMiddle origin destination
             in case Map.lookup middle board of
                    Just (p, _) -> p == next player
                    _           -> False

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


insertBoard :: Board -> Position -> (Player, PieceType) -> Board
insertBoard board position piece =
    Map.insert position piece board

deleteBoard :: Board -> Position -> Board
deleteBoard board position = Map.delete position board
