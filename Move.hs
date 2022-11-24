module Move where

import Types (Board, Coordinate, Player(..))
import Utils (piece, size, king, getCoordinatesBetween, diagonalDistance)


-- change the value of an item in the list to a new value
change :: Int -> a -> [a] -> [a]
change n x xs = [if i == n then x else x' | (x', i) <- zip xs [0..]]

move :: Board -> (Coordinate, Coordinate) -> Board
move b ((x1, y1), (x2, y2)) =
    if diagonalDistance (x1, y1) (x2, y2) == 2
        then jump     b ((x1, y1), (x2, y2))
        else dontJump b ((x1, y1), (x2, y2))

dontJump :: Board -> (Coordinate, Coordinate) -> Board
dontJump b ((x1, y1), (x2, y2)) =
    change y1 (change x1 E (b' !! y1)) b'
    where b' = change y2 (change x2 p' (b !! y2)) b
          p = piece b (x1, y1)
          p' = if otherEnd p (x2, y2)
               then king $ p
               else p


jump :: Board -> (Coordinate, Coordinate) -> Board
jump b ((x1, y1), (x2, y2)) =
    change yb (change xb E (b' !! yb)) b'
    where b' = dontJump b ((x1, y1), (x2, y2))
          (xb, yb) = getCoordinatesBetween (x1, y1) (x2, y2)


otherEnd :: Player -> Coordinate -> Bool
otherEnd p (_, y) =
    case king p of
        WW -> y == size - 1
        BB -> y == 0