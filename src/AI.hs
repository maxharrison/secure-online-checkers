module AI where

import Valid (valid)
import Types (Board, Player(..), Coordinate, Move, Tree(..))
import Utils (whosWon, won, piecesPlayer, pieces, startBoard, next, countPiece, countPlayer, king, test1Board)
import Utils (test1Board, winingTest, kingTest__)
import Move (move)
import Display (displayBoard, showBoard)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Data.List (group)
import Control.Monad (forM_)
import System.Environment
import Score (score_count)
import Input (getCoordinates)


minimax :: (Board -> Int) -> Tree (Board, Player, Move) -> Tree ((Board, Player, Move), Int)
minimax scoreF (Node (b, p, m) []) =
    (Node ((b, p, m), scoreF b) [])
minimax scoreF (Node (b, p, m) trees) =
    Node ((b, p, m), miniOrMax scores) newTrees
        where newTrees = map (minimax scoreF) trees
              scores = [score | Node ((_, _, _), score) _ <- newTrees]
              miniOrMax = if p == W then minimum else maximum


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


-- Returns a tree of the depth given
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


bestMove :: Int -> (Board -> Int) -> Board -> Player -> Maybe (Board, Player, Move)
bestMove depth sf b p = if null moves then Nothing else Just (random moves)
    where moves = [bpm | Node (bpm, s) _ <- ts, s == best]
          tree = prune depth (gameTree b p em)
          Node ((_, _, _), best) ts = minimax sf tree
          em = ((0, 0), (0, 0))


random :: [a] -> a
random xs = xs !! unsafePerformIO (randomRIO (0, length xs - 1))


playAI :: Board -> Player -> IO()
playAI board W
    | whosWon board == Just B = do displayBoard board
                                   putStrLn "\nPlayer B wins!\n"
    | whosWon board == Just W = do displayBoard board
                                   putStrLn "\nPlayer W wins!\n"
    | otherwise = do -- WHITE IS USER
        putStrLn $ "\nWhite (you):"
        displayBoard board
        playerMove <- getCoordinates
        if valid board W playerMove
            then let newBoard = move board playerMove
                 in playAI newBoard B
            else do putStrLn "Invalid move"
                    playAI board W
playAI board B
    | whosWon board == Just B = do displayBoard board
                                   putStrLn "\nPlayer B wins!\n"
    | whosWon board == Just W = do displayBoard board
                                   putStrLn "\nPlayer W wins!\n"
    | otherwise = do -- BLACK IS AI
        putStrLn $ "\nBlack (AI):"
        displayBoard board
        putStrLn "\nAI is thinking..."
        let aiMove = bestMove {-depth:-}3 {-scoring function:-}score_count {-board:-}board {-Player:-}B
        case aiMove of
            Nothing -> do putStrLn "Invalid move"
                          playAI board B
            Just (newBoard, _, _) -> playAI newBoard W