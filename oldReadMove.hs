-- this is for sake of documentation in the future


-- remove all spaces from string
removeSpaces :: String -> String
removeSpaces s = filter (\c -> c /= ' ') s

-- get the first and last characters from a string,
-- if the length of the string is 2s
getOneTwo :: String -> Maybe (Char, Char)
getOneTwo s = if (length s == 2)
   then Just (head s, last s)
   else Nothing

-- get the XY coordinates in terms of a 2D matrix of lists
-- the letter is always the X, and number is always Y
-- "B5" = (4, 1)
-- "5B" = (4, 1)
getXY :: (Char, Char) -> Maybe (Int, Int)
getXY (one, two)
    | isLetter one && isDigit  two = Just (digToInt two, charToInt one)
    | isDigit  one && isLetter two = Just (digToInt one, charToInt two)
    | otherwise                    = Nothing

charToInt :: Char -> Int
charToInt c = (ord $ toUpper c) - 65

digToInt :: Char -> Int
digToInt c = (digitToInt c) - 1

readMove :: String -> Maybe (Int, Int)
readMove s = do
   (one, two) <- getOneTwo $ removeSpaces s
   getXY (one, two)
