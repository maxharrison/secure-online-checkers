module AI where

-- Haskell modules
import qualified Data.Map as Map
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

-- My modules
import GameState
import Valid



data Tree a = Node a [Tree a]




moves board player = getAllValidBoardsRoutes board player

-- Returns a tree of all the possible boards, with their move
game_tree :: Board -> Player -> Route -> Tree (Board, Player, Route)
game_tree b p r = Node (b, p, r) [game_tree b' (next p) r | (b', r) <- moves b p]


best_move :: Int -> (Board -> Int) -> Board -> Player -> Maybe (Board, Player, Route)
best_move depth sf b p = if null moves then Nothing else Just (random moves)
    where moves = [bpr | Node (bpr, s) _ <- ts, s == best]
          tree = prune depth (game_tree b p er)
          Node ((_, _, _), best) ts = minimax sf tree
          er = []

score_count :: Board -> Int
score_count board = b - w
    where w = countPlayerPieces board White
          b = countPlayerPieces board Black



random :: [a] -> a
random xs = xs !! unsafePerformIO (randomRIO (0, length xs - 1))


minimax :: (Board -> Int) -> Tree (Board, Player, Route) -> Tree ((Board, Player, Route), Int)
minimax sf (Node (b, p, r) []) =
    (Node ((b, p, r), sf b) [])
minimax sf (Node (b, p, r) ts)
    | p == White = Node ((b, p, r), minimum ss) ts'
    | p == Black = Node ((b, p, r), maximum ss) ts'
        where ts' = map (minimax sf) ts
              ss = [s | Node ((_, _, _), s) _ <- ts']

-- Returns a tree of the depth given
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

