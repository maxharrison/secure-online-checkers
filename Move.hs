module Move where

import Types


getPiece :: Board -> Coordinate -> Piece
getPiece b (x, y) = (b !! (y)) !! (x)

whosPiece :: Piece -> Maybe Player
whosPiece (Just (p, _)) = Just p
whosPiece Nothing       = Nothing

getCoordinatesBetween :: Coordinate -> Coordinate -> Coordinate
getCoordinatesBetween (x1, y1) (x2, y2) = (x1 + (x2 - x1) `div` 2, y1 + (y2 - y1) `div` 2)

validBasic :: Board -> Move -> Bool
validBasic b m = isUserMovingOwnPiece b m
              && isDestinationOnBoard b m
              && isDestinationEmpty b m
              && isUserMovingForwardOrKing m

 
isUserMovingForwardOrKing :: Move -> Bool
isUserMovingForwardOrKing (p, (_, y1), (_, y2)) =
    case p of
        Just (W, False) -> y1 < y2
        Just (B, False) -> y1 > y2
        otherwise       -> True

isUserMovingOwnPiece :: Board -> Move -> Bool
isUserMovingOwnPiece b (p, c, _) =
    (getPiece b c) == p


isDestinationOnBoard :: Board -> Move -> Bool
isDestinationOnBoard b (_, _, (x, y)) =
    x >= 0 && x < cols && y >= 0 && y < rows

  
isDestinationEmpty :: Board -> Move -> Bool
isDestinationEmpty b (_, _, c2) = getPiece b c2 == Nothing



diagonalDistance :: Move -> Maybe Int
diagonalDistance (_, (x1, y1), (x2, y2)) =
    if x == y
        then Just x
        else Nothing
    where x = abs (x1 - x2)
          y = abs (y1 - y2)
    


-- change the value of an item in the list to a new value
change :: Int -> a -> [a] -> [a]
change n x xs = [if i == n then x else x' | (x', i) <- zip xs [0..]]


isMovingKing :: Move -> Bool
isMovingKing (Just (_, True), _, _) = True
isMovingKing _                      = False


turnPieceToKingIfAtOtherEnd :: Move -> Move
turnPieceToKingIfAtOtherEnd (p, c1, c2) =
    case p of
        Just (W, False) -> if snd c2 == rows - 1
                           then (Just (W, True), c1, c2)
                           else (p, c1, c2)
        Just (B, False) -> if snd c2 == 0
                           then (Just (B, True), c1, c2)
                           else (p, c1, c2)
        otherwise       -> (p, c1, c2)

movePiece :: Board -> Move -> Maybe Board
movePiece b m =
    if not $ validBasic b m
        then Nothing -- not valid move
        else let m' = turnPieceToKingIfAtOtherEnd m
             in case diagonalDistance m of
                     Just 1 -> Just (noJumpBoard b m') -- no jump
                     Just 2 -> Just (jumpBoard b m')   -- jumping
                     otherwise -> Nothing -- not moving diagnoally 1 or 2 places


noJumpBoard :: Board -> Move -> Board
noJumpBoard b (p, (x1, y1), (x2, y2)) =
    change y1 (change x1 Nothing (b' !! y1)) b'
    where b' = change y2 (change x2 p (b !! y2)) b


jumpBoard :: Board -> Move -> Board
jumpBoard b (p, (x1, y1), (x2, y2)) =
    change y1 (change x1 Nothing (b'' !! y1)) b''
    where b' = change y2 (change x2 p (b !! y2)) b
          (xm, ym) = getCoordinatesBetween (x1, y1) (x2, y2)
          b'' = change ym (change xm Nothing (b' !! ym)) b'
