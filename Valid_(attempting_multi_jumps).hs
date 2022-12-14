module Valid where

import Types (Board, Player(..), Coordinate, Move)
import Utils (piece, size, king, startBoard, next, getCoordinatesBetween, diagonalDistance)
import Move (jump)



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
    (diagonalDistance m == 2 &&
    isPieceBetweenOtherPlayer b p m) 


--canMultipleJump :: Board -> Player -> Move -> Bool
--canMultipleJump board player (origin, destination) =
--    let possibleJumps = getPossibleJumps ----
--    in isReachable destination jumps




-- HELP FROM https://stackoverflow.com/questions/37511246/haskell-checkers-how-to-write-a-function-that-returns-a-list-of-possible-jumps
singleJumps :: Player -> Coordinate -> Board -> [(Coordinate, Board)]
singleJumps p (x, y) b = [(destination, b') | destination <- [(x+2, y+2), (x+2, y-2), (x-2, y+2), (x-2, y-2)], 
                                        isPieceBetweenOtherPlayer b p ((x, y), destination),
                                        b' <- [jump b ((x, y), destination)]]

-- HELP FROM https://stackoverflow.com/questions/37511246/haskell-checkers-how-to-write-a-function-that-returns-a-list-of-possible-jumps
multiJumps :: Player -> Coordinate -> Board -> [([Coordinate], Board)]
{- 
as list comphehension
for each possible single jump (rc, b):
  for each possible multi jump (path, b') starting from (rc,b):
    return the path (rc:path) and ending board configuration b'
-}
multiJumps p origin b = [([dst] ++ dsts, b'') | (dst, b')   <- singleJumps p origin b,
                                                (dsts, b'') <- multiJumps p dst    b']




--
---------------------------------------

valid :: Board -> Player -> Move -> Bool
valid b p (c1, c2) =
    if isKing b c1 
        then validKing b p (c1, c2)
        else validPawn b p (c1, c2)
