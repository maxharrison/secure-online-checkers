module Valid where

import Types (Board, Player(..), Coordinate, Move)
import Utils (piece, size, king, startBoard, next, getCoordinatesBetween, diagonalDistance)



isPieceBetweenOtherPlayer :: Board -> Player -> Move -> Bool
isPieceBetweenOtherPlayer b p m =
    king (piece b cBetween) == king (next p)
    where cBetween = getCoordinatesBetween m


isUserMovingForward :: Player -> Move -> Bool
isUserMovingForward W ((_, y1), (_, y2)) = y1 < y2
isUserMovingForward B ((_, y1), (_, y2)) = y1 > y2



isDiagonally :: Move -> Bool
isDiagonally ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

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

validPawn :: Board -> Player -> Move -> Bool
validPawn b p m =
    validKing b p m &&
    isUserMovingForward p m



validKing :: Board -> Player -> Move -> Bool
validKing b p (c1, c2) =
    isOwnPiece b p c1 &&
    isDestinationOnBoard b c2 &&
    isEmpty b c2 &&
    isDiagonally (c1, c2) &&
    (diagonalDistance (c1, c2) == 1 || canJump b p (c1, c2))


canJump :: Board -> Player -> Move -> Bool
canJump b p m =
    diagonalDistance m == 2 &&
    isPieceBetweenOtherPlayer b p m


valid :: Board -> Player -> Move -> Bool
valid b p (c1, c2) =
    if isKing b c1 
        then validKing b p (c1, c2)
        else validPawn b p (c1, c2)
