module Win where

import Types

whosWon :: Board -> Maybe Player
whosWon b = if (countBoard W b) == 0
            then Just B
            else if (countBoard B b) == 0
                 then Just W
                 else Nothing


countRow :: Player -> Row -> Int
countRow p r = length (filter (== one) r) + length (filter (== two) r)
    where one = Just (p, True)
          two = Just (p, False)

countBoard :: Player -> Board -> Int
countBoard p b = sum $ map (countRow p) b 