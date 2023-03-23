module Binary where

import Data.Char (ord, toLower, chr)


----------------------------------------------------------------
--                           Types                            --
----------------------------------------------------------------


type Bit = Bool

type Binary = [Bit]

showBinary :: Binary -> String
showBinary binary = concatMap (\b -> if b then "1" else "0") binary



----------------------------------------------------------------
--                           Conversion                       --
----------------------------------------------------------------


-- can input hex by prepending 0x to the start of the input string
--numberToBinary :: String -> Binary
--numberToBinary = intToBinary . read 

intToBinary :: Int -> Binary
intToBinary 0 = []
intToBinary n = intToBinary (div n 2) ++ [(mod n 2 == 1)]

pad :: Int -> Binary -> Binary
pad n xs = replicate (n - length xs) False ++ xs

binaryToInt :: Binary -> Int
binaryToInt [] = 0
binaryToInt (b:bs) = b' * 2^(length bs) + binaryToInt bs
  where b' = if b then 1 else 0

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = as : group n bs 
  where (as, bs) = splitAt n xs

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

getIndexes :: [Int] -> [a] -> [a]
getIndexes [] _ = []
getIndexes (n:ns) xs = xs !! n : getIndexes ns xs

----------------------------------------------------------------

stringToBinary :: String -> Binary
stringToBinary = concatMap charToBinary

charToBinary :: Char -> Binary
charToBinary = pad 8 . intToBinary . ord

--binaryToString :: Binary -> String
--binaryToString = map binaryToChar . group 8

--binaryToChar :: Binary -> Char
--binaryToChar = chr . binaryToInt


----------------------------------------------------------------
--                           Operations                       --
----------------------------------------------------------------

xor :: Binary -> Binary -> Binary
xor bsx bsy = [if x == y then False else True | (x, y) <- zip bsx bsy]

leftShift :: Int -> Binary -> Binary
leftShift n xs = take (length xs) . drop n $ cycle xs


