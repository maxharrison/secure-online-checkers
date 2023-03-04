module Binary where

import Data.Bits

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
numberToBinary :: String -> Binary
numberToBinary = intToBinary . read 

-- if smaller than a byte it will get padded out to a byte
--numberToByte :: String -> Binary
--numberToByte = pad 8 . numberToBinary

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

stringToBinary :: String -> Binary
stringToBinary = concatMap charToBinary

charToBinary :: Char -> Binary
charToBinary c = (pad 8) $ reverse $ go (ord c) []
  where
    go 0 acc = acc
    go n acc = go (n `div` 2) ((n `mod` 2 == 1) : acc)

binaryToChar :: [Bool] -> Char
binaryToChar bs = chr $ go $ reverse $ (pad 8) bs
  where
    go [] = 0
    go (b:bs) = (if b then 1 else 0) + 2 * go bs


----------------------------------------------------------------
--                           Operations                       --
----------------------------------------------------------------

xor' :: Binary -> Binary -> Binary
xor' bsx bsy = [if x == y then False else True | (x, y) <- zip bsx bsy]

leftShift :: Int -> Binary -> Binary
leftShift n xs = take (length xs) . drop n $ cycle xs



----------------------------------------------------------------


{-bitsToString :: Binary -> String
bitsToString bs = map (intsToChar . bitsToInt) $ group 8 bs

bitsToInt :: Binary -> Int
bitsToInt [] = 0
bitsToInt (b:bs) = b * 2^(length bs) + bitsToInt bs

intsToChar :: Int -> Char
intsToChar n | n >= 0 && n<= 9 = head $ show n
             | n >= 10 && n<= 35 = chr $ n + 87
             | n == 36 = ' '
             | n == 37 = ','
             | otherwise = chr n --error "Not valid"-}