module AI where

import Valid (valid)
import Types (Board, Player(..), Coordinate, Move, Tree(..))
import Utils (whosWon, won, piecesPlayer, pieces, startBoard, next, countPiece, countPlayer, king, test1Board)
import Utils (test1Board, winingTest, kingTest__)
import Move (move)
import Display (displayBoard, showBoard)


minimax :: Tree (Board, Player, Move) -> Tree ((Board, Player, Move), Int)
minimax (Node (b, p, m) []) =
    (Node ((b, p, m), score b) [])
minimax (Node (b, p, m) ts)
    | p == W = Node ((b, p, m), minimum ss) ts'
    | p == B = Node ((b, p, m), maximum ss) ts'
        where ts' = map minimax ts
              ss = [s | Node ((_, _, _), s) _ <- ts']



 -- Returns all the possible moves a player can do, with the resultant board
moves :: Board -> Player -> [(Board, Move)]
moves b p
    | whosWon b == Nothing = [
        (move b (c1, c2), (c1, c2)) |
        c1 <- piecesPlayer b p,
        c2 <- pieces b E,
        valid b p (c1, c2)]
    | otherwise = []



-- Returns a tree of all the possible boards, with their move
gameTree :: Board -> Player -> Move -> Tree (Board, Player, Move)
gameTree b p m = Node (b, p, m) [gameTree b' (next p) m | (b', m) <- moves b p]


depth :: Int
depth = 4

-- Returns a tree of the depth given
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


bestMove :: Board -> Player -> (Board, Player, Move)
bestMove b p = head [bpm | Node (bpm, s) _ <- ts, s == best]
    where tree = prune depth (gameTree b p em)
          Node ((_, _, _), best) ts = minimax tree
          em = ((0, 0), (0, 0))


-- evaluation function
-- positive means B is winning, negative means W is winning
score :: Board -> Int
score b = ((xb*2) + yb) - ((xw*2) + yw)
    where xb = countPiece b B
          yb = countPiece b BB
          xw = countPiece b W
          yw = countPiece b WW

          
play :: Board -> Player -> IO ()
play b p = do
    let (b', p', (c1, c2)) = bestMove b p
    putStrLn ""
    putStrLn $ show p ++ " : " ++ show c1 ++ " -> " ++ show c2
    displayBoard b'
    play b' (next p)


main :: IO ()
main = do
    let board = startBoard
    displayBoard board
    play board W
