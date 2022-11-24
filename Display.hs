module Display where

import Data.Char (chr)
import Types (Board, Row, Player(..))
import Utils (size)


instance Show Player where
 -- show :: Player -> String
    show E  = "-"
    show W  = "♦"
    show WW = "W"
    show B  = "♢"
    show BB = "B"

showRow :: Row -> String
showRow r = concat [show p ++ " " | p <- r]

showBoard :: Board -> String
showBoard b = concat [showRow r ++ "\n" | r <- b]

displayBoard :: Board -> IO ()
displayBoard b = do
    let rows = lines $ showBoard b
    let numbers = [n | n <- [1..size]]
    let letters = [c | c <- ['A'..chr $ 64+size]]
    let string = concat [show n ++ " " ++ r ++ "\n" | (n, r) <- zip numbers rows]
                 ++ "  " ++ concat [[l, ' '] | l <- letters]
    putStrLn string


