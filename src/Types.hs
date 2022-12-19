module Types where

type Board = [Row]

type Row = [Player]

data Player = W | WW | E | B | BB
    deriving (Eq, Ord)

type Coordinate = (Int, Int)

type Move = (Coordinate, Coordinate)

data Tree a = Node a [Tree a]
    deriving Show