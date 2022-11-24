module Types where

type Board = [Row]


type Row = [Piece]


data Player = W | B
   deriving (Eq, Show)

-- Nothing for empty piece on board
-- (W, False) for regular white piece
-- (B, True) for king black piece
type Piece = Maybe (Player, Bool)



-- dont know whether this should be the index of the piece in the matrix,
-- or the X Y coordinates from the users' perspective (either both ints, or one char and one int)
-- also dont know whether the board should be a list of rows, or a list of columns
-- or if the board should be like a bitmap
type Coordinate = (Int, Int)

type Move = (Piece, Coordinate, Coordinate)

rows :: Int
rows = 8

cols :: Int
cols = 8