module Valid where

import Types (Board, Coordinate, Player(..))
import Utils (piece, size, king, startBoard, next, getCoordinatesBetween, diagonalDistance)



isPieceBetweenOtherPlayer :: Board -> Player -> Coordinate -> Coordinate -> Bool
isPieceBetweenOtherPlayer b p c1 c2 =
    king (piece b cBetween) == king (next p)
    where cBetween = getCoordinatesBetween c1 c2




isUserMovingForward :: Player -> Coordinate -> Coordinate -> Bool
isUserMovingForward p (_, y1) (_, y2) = case p of
    W         -> y1 < y2
    B         -> y1 > y2
    otherwise -> True


isDiagonally :: Coordinate -> Coordinate -> Bool
isDiagonally (x1, y1) (x2, y2) = abs (x1 - x2) == abs (y1 - y2)

isEmpty :: Board -> Coordinate -> Bool
isEmpty b c = piece b c == E


isDestinationOnBoard :: Board -> Coordinate -> Bool
isDestinationOnBoard b (x, y) =
    x >= 0 && x < size && y >= 0 && y < size

isOwnPiece :: Board -> Player -> Coordinate -> Bool
isOwnPiece b p c = piece b c == p || piece b c == king p

isKing :: Board -> Coordinate -> Bool
isKing b (x, y) | piece b (x, y) == WW = True
                | piece b (x, y) == BB = True
                | otherwise            = False

validPawn :: Board -> Player -> Coordinate -> Coordinate -> Bool
validPawn b p c1 c2 =
    validKing b p c1 c2 &&
    isUserMovingForward p c1 c2



validKing :: Board -> Player -> Coordinate -> Coordinate -> Bool
validKing b p c1 c2 =
    isOwnPiece b p c1 &&
    isDestinationOnBoard b c2 &&
    isEmpty b c2 &&
    isDiagonally c1 c2 &&
    (diagonalDistance c1 c2 == 1 || canJump b p c1 c2)


canJump :: Board -> Player -> Coordinate -> Coordinate -> Bool
canJump b p c1 c2 =
    diagonalDistance c1 c2 == 2 &&
    isPieceBetweenOtherPlayer b p c1 c2


valid :: Board -> Player -> Coordinate -> Coordinate -> Bool
valid b p c1 c2 =
    if isKing b c1 
        then validKing b p c1 c2
        else validPawn b p c1 c2
