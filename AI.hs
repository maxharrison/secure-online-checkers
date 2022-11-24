module AI where

import Valid (valid)
import Types (Board, Player(..), Coordinate)
import Utils (whosWon, won, piecesPlayer, pieces, startBoard, next, countPiece, countPlayer, king, test1Board)
import Move (move)
import Display (displayBoard, showBoard)




minimax :: Tree ((Board, Move), Player) -> Tree ((Board, Move), Player)
minimax (Node ((b, m), p) []) 
   | won b W  = Node ((b, m), W) []
   | won b B  = Node ((b, m), B) []
   | otherwise = Node ((b, m), E) []
minimax (Node ((b, m), p) ts)
   | p == W = Node ((b, m), minimum ps) ts'
   | p == B = Node ((b, m), maximum ps) ts'
      where ts' = map minimax ts
            ps = [p' | Node (_, p') _ <- ts']


 -- Returns all the possible moves a player can do, with the resultant board
moves :: Board -> Player -> [(Board, Move)]
moves b p
    | whosWon b == Nothing = [
        (move b (c1, c2), (c1, c2)) |
        c1 <- piecesPlayer b p,
        c2 <- pieces b E,
        valid b p c1 c2]
    | otherwise = []



-- Returns a tree of all the possible boards, with their move
gameTree :: Board -> Player -> Move -> Tree ((Board, Move), Player)
gameTree b p m = Node ((b, m), p) [gameTree b' (next p) m | (b', m) <- moves b p]


depth :: Int
depth = 3

-- Returns a tree of the depth given
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


bestMove :: Board -> Player -> (Board, Move)
bestMove b p = head [bm | Node ((bm), p') _ <- ts, p' == best]
    where tree = prune depth (gameTree b p em)
          Node ((_, _), best) ts = minimax tree
          em = ((0, 0), (0, 0))

score :: Board -> Player -> Float
score b p = (fromIntegral x) + ((fromIntegral y) / 2)
    where x = countPiece b p
          y = countPiece b (king p)
          

          
play :: Board -> Player -> IO ()
play b p = do
    let (b', (c1, c2)) = bestMove b p
    putStrLn ""
    putStrLn $ show p ++ " : " ++ show c1 ++ " -> " ++ show c2
    displayBoard b'
    play b' (next p)


main :: IO ()
main = do
    let board = startBoard
    displayBoard board
    play board W
