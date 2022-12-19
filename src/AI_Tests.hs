module AI_Tests where

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

import Score




minimax :: (Board -> Int) -> Tree (Board, Player, Move) -> Tree ((Board, Player, Move), Int)
minimax sf (Node (b, p, m) []) =
    (Node ((b, p, m), sf b) [])
minimax sf (Node (b, p, m) ts)
    | p == W = Node ((b, p, m), minimum ss) ts'
    | p == B = Node ((b, p, m), maximum ss) ts'
        where ts' = map (minimax sf) ts
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





writeResults :: String -> Player -> Int -> IO ()
writeResults fn E n = do
    appendFile ("results/"++fn) $ "D:" ++ show n ++ "\n"
writeResults fn p n = do
    appendFile ("results/"++fn) $ show p ++ ":" ++ show n ++ "\n"



-- first sf is W, second sf is B
simulate :: Int -> Int -> String -> (Board -> Int) -> (Board -> Int) -> Board -> Player -> IO ()
simulate _ 250 fn _ _ b p = 
    if score_count b == 0 then writeResults fn (E) 250
        else if score_count b > 0
            then writeResults fn (king B) 250
            else writeResults fn (king W) 250
simulate depth n fn sf1 sf2 b p
    | whosWon b == Just B = do displayBoard b
                               writeResults fn (king B) n
    | whosWon b == Just W = do displayBoard b
                               writeResults fn (king W) n
    | otherwise = do
        let sf = if p == W then sf1 else sf2
        case bestMove depth sf b p of
            Nothing -> writeResults fn E n
            Just (b', p', (c1, c2)) -> do
                putStrLn ""
                putStrLn $ show p ++ " : " ++ show c1 ++ " -> " ++ show c2
                displayBoard b'
                simulate depth (n+1) fn sf1 sf2 b' (next p)


-- Returns number of wins for each function
simulateWrite :: Int -> String -> Int -> (Board -> Int) -> (Board -> Int) -> Board -> IO ()
simulateWrite depth fn n sf1 sf2 b = do
    forM_ [1..(n`div`2)] $ \_ -> do
        simulate depth 0 fn sf1 sf2 b W
        simulate depth 0 fn sf1 sf2 b B
    return ()




main2 :: IO ()
main2 = do
    (w_sf:b_sf:depth:interations:_) <- getArgs
    --let w_sf = "score_nothing"
    --let b_sf = "score_count_basic"
    --let depth = "1"
    --let interations = "2"
    let fileName = "D:" ++ depth ++ "_W:" ++ w_sf ++ "_B:" ++ b_sf ++ ".txt"
    simulateWrite (read depth) fileName (read interations) (chooseScoreFunction w_sf) (chooseScoreFunction b_sf) startBoard
