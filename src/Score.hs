module Score where


import Types (Board, Player(..))
import Utils (countPiece, king, whosWon)

-- positive means B is winning, negative means W is winning
chooseScoreFunction :: String -> (Board -> Int)
chooseScoreFunction "score_nothing" = score_nothing
chooseScoreFunction "score_count" = score_count
chooseScoreFunction "score_distance" = score_distance
chooseScoreFunction "score_win" = score_win
chooseScoreFunction "score_count_basic" = score_count_basic
chooseScoreFunction _ = score_nothing





score_nothing :: Board -> Int
score_nothing b = 0

-- if B wins then equal 1, if W wins then equal -1, else 0
score_win :: Board -> Int
score_win b = case whosWon b of
                Just B -> 1
                Just W -> -1
                Nothing -> 0

score_count :: Board -> Int
score_count b = (bp + (bk * 2)) - (wp + (wk * 2))
    where bp = countPiece b B
          bk = countPiece b BB
          wp = countPiece b W
          wk = countPiece b WW

score_count_basic :: Board -> Int
score_count_basic b = (bp + bk) - (wp + wk)
    where bp = countPiece b B
          bk = countPiece b BB
          wp = countPiece b W
          wk = countPiece b WW


countPieceRow :: Board -> Player -> Int -> Int
countPieceRow b p r = length [c | c <- b !! r, c == p] + (2 * length [c | c <- b !! r, c == king p])

-- counts how far up the board each piece is and adds it together
score_distance :: Board -> Int
score_distance b = round ((scoreB - scoreW)*100)
    where rows = [0..7]
          scoreB = sum [fromIntegral (countPieceRow b B r) * (1-((fromIntegral r)/7))| r <- rows]
          scoreW = sum [fromIntegral (countPieceRow (reverse b) W r) * (1-((fromIntegral r)/7))| r <- reverse rows]

