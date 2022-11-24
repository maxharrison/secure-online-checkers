module Utils where

import Types (Board, Row, Player(..), Coordinate)

size :: Int
size = 8

startBoard :: Board
startBoard = [[E  , W  , E  , W  , E  , W  , E  , W  ],
              [W  , E  , W  , E  , W  , E  , W  , E  ],
              [E  , W  , E  , W  , E  , W  , E  , W  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [B  , E  , B  , E  , B  , E  , B  , E  ],
              [E  , B  , E  , B  , E  , B  , E  , B  ],
              [B  , E  , B  , E  , B  , E  , B  , E  ]]

test1Board :: Board
test1Board = [[E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , W  , E  , E  , E  ],
              [E  , E  , E  , B  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ],
              [E  , E  , E  , E  , E  , E  , E  , E  ]]

piecesPlayer :: Board -> Player -> [Coordinate]
piecesPlayer b p = (pieces b p) ++ (pieces b $ king p)

pieces :: Board -> Player -> [Coordinate]
pieces b p = [(x, y) |
              (r, y) <- zip b [0..],
              (p', x) <- zip r [0..],
              p' == p] 

king :: Player -> Player
king W = WW
king B = BB
king p = p

countPlayer :: Board -> Player -> Int
countPlayer b p = length $ piecesPlayer b p

countPiece :: Board -> Player -> Int
countPiece b p = length $ pieces b p

whosWon :: Board -> Maybe Player
whosWon b | countPlayer b W == 0 = Just B
          | countPlayer b B == 0 = Just W
          | otherwise            = Nothing

piece :: Board -> Coordinate -> Player
piece b (x, y) = b !! y !! x

next :: Player -> Player
next W = B 
next B = W

getCoordinatesBetween :: Coordinate -> Coordinate -> Coordinate
getCoordinatesBetween (x1, y1) (x2, y2) = (x1 + (x2 - x1) `div` 2, y1 + (y2 - y1) `div` 2)

diagonalDistance :: Coordinate -> Coordinate -> Int
diagonalDistance (x1, y1) (x2, y2) = abs (x1 - x2)