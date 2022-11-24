module Display where

import Types
import Data.Char

showPiece :: Piece -> Char
showPiece Nothing         = '-'
showPiece (Just (W, False)) = '♦'
showPiece (Just (B, False)) = '♢'
showPiece (Just (W, True))  = 'W'
showPiece (Just (B, True))  = 'B'

showRow :: Row -> String
showRow r = concat [ [showPiece c, ' '] | c <- r]

showBoard ::Board -> Player -> IO ()
showBoard b p = putStr ((concat grid) ++ letters)
   where grid = f [show c ++ " " ++ r ++ "\n" |  c <- ([1..cols]),
                                                 r <- [showRow (b !! (c-1))]]
         letters = "  " ++ (concat [[c, ' '] | c <- ['A'..chr $ 64+rows]]) ++ "\n"
         f = if p == B then id else reverse