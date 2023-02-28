module Feistel where

import Data.Char (ord, toLower, chr)
import Debug.Trace

-- would really like to have types for fixed length lists so that i can make sure functions take for e.g. 64 bits as input
type Key = Bits
type Bits = [Int]



plainText = [0, 0, 0, 1, 0, 0, 0, 1,
             0, 0, 1, 0, 0, 0, 1, 0,
             0, 0, 1, 1, 0, 0, 1, 1,
             0, 1, 0, 0, 0, 1, 0, 0,
             0, 1, 0, 1, 0, 1, 0, 1,
             0, 1, 1, 0, 0, 1, 1, 0,
             0, 1, 1, 1, 0, 1, 1, 1,
             1, 0, 0, 0, 1, 0, 0, 0]

key = [0, 1, 1, 1, 0, 1, 0, 1,
       0, 0, 1, 0, 1, 0, 0, 0,
       0, 1, 1, 1, 1, 0, 0, 0,
       0, 0, 1, 1, 1, 0, 0, 1,
       0, 1, 1, 1, 0, 1, 0, 0,
       1, 0, 0, 1, 0, 0, 1, 1,
       1, 1, 0, 0, 1, 0, 1, 1,
       0, 1, 1, 1, 0, 0, 0, 0]


target = [1, 0, 1, 1, 0, 1, 0, 1,
          0, 0, 1, 0, 0, 0, 0, 1,
          1, 0, 0, 1, 1, 1, 1, 0,
          1, 1, 1, 0, 1, 0, 0, 0,
          0, 0, 0, 1, 1, 0, 1, 0,
          1, 0, 1, 0, 0, 1, 1, 1,
          0, 1, 0, 0, 1, 0, 0, 1,
          1, 0, 0, 1, 1, 1, 0, 1]


cipherText = encryptBlock key plainText

test :: IO ()
test = do
  putStrLn $ "PT Binary: " ++ show plainText
  putStrLn $ "PT STRING: " ++ bitsToString plainText
  putStrLn $ "TG Binary: " ++ show target
  --putStrLn $ "TG STRING: " ++ bitsToString target
  putStrLn $ "CT Binary: " ++ show cipherText
  print $ target == cipherText
  --putStrLn $ bitsToString $ decryptBlock key cipherText
  --putStrLn $ "CT STRING: " ++ bitsToString cipherText
  --print $ tripleDesEncryptBlock (key ++ key ++ key) plainText

  let keys = keySchedule key
  let (left, right) = halves plainText
  print $ (f (keys !! 0) right)



------------------------------------------------------------------
--                        Main Functions                        --
------------------------------------------------------------------


tripleDesEncryptBlock :: Key -> Bits -> Bits
tripleDesEncryptBlock key plainTextBlock =
  let key1 = take 64 key
      key2 = (take 64 . drop 64) key
      key3 = drop 128 key
  in (encryptBlock key3 . decryptBlock key2 . encryptBlock key1) plainTextBlock

encryptBlock :: Key -> Bits -> Bits
encryptBlock key plainTextBlock =
  let keys = keySchedule key
  in (finalPermutation . uncurry (++) . des keys . halves . initialPermutation) plainTextBlock

decryptBlock :: Key -> Bits -> Bits
decryptBlock key plainTextBlock = plainTextBlock
  
--  let keys = reverse $ keySchedule key
--  in (finalPermutation . uncurry (++) . des keys . halves . initialPermutation) plainTextBlock


des :: [Key] -> (Bits, Bits) -> (Bits, Bits)
des [] (l, r) = (r, l)
des (k:ks) (l, r) =
  des ks $ feistel k (l, r)


feistel :: Key -> (Bits, Bits) -> (Bits, Bits)
feistel k (l, r) =
  let l' = r
      r' = xor (f k r) l
  in (l', r')

initialPermutation = applyTable initialPermutationTable

finalPermutation = applyTable finalPermutationTable

------------------------------------------------------------------
--                          F Function                          --
------------------------------------------------------------------

-- k : the key, length 64 bits
-- bs : the bits being run in the f function, 64 bits
f :: Key -> Bits -> Bits
f k bs = (permuation . sboxs . expansion) bs--(permuation . sboxs . (xor k) . expansion) bs


expansion :: Bits -> Bits
expansion bs = applyTable expansionTable bs


-- 48 bit input, 32 bit output
sboxs :: Bits -> Bits
sboxs bs =
  let boxes = map applySbox [sbox1, sbox2, sbox3, sbox4, sbox5, sbox6, sbox7, sbox8]
  in concat $ zipWith ($) boxes $ group 6 bs


permuation :: Bits -> Bits
permuation bs = applyTable permutationTable bs


-- 6 bit input, 4 bit output
applySbox :: [[Bits]] -> Bits -> Bits
applySbox sbox bs =
  let row = bitsToInt $ bs!!0 : [bs!!5]
      column = bitsToInt $ bs!!1 : bs!!2 : bs!!3 : [bs!!4]
  in (sbox!!row)!!column





------------------------------------------------------------------
--                        Key Schedule                          --
------------------------------------------------------------------

keySchedule :: Key -> [Key]
keySchedule key =
  roundKeys shifts left right
  where shifts = [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]
        (left, right) = (halves . initialKeyPermutation) key



roundKeys :: [Int] -> Key -> Key -> [Key]
roundKeys [] _ _ = []
roundKeys (shift:shifts) left right =
  let left' = leftShift shift left
      right' = leftShift shift right
  in roundKeyPermuation (left' ++ right') : roundKeys shifts left' right'


  --for each of these just take the number of shifts at the head of the list
  --shift the things, make a subkey with that (and do something with that, idk how to handle that yet, maybe append it to a new list)
  --then recusivly call yourself

roundKeyPermuation = applyTable roundKeyPermuationTable

initialKeyPermutation = applyTable initialKeyPermutationTable

------------------------------------------------------------------
--                        Helper Functions                      --
------------------------------------------------------------------

xor :: Bits -> Bits -> Bits
xor bsx bsy = [if x == y then 0 else 1 | (x, y) <- zip bsx bsy]

halves :: Bits -> (Bits, Bits)
halves bs = splitAt (length bs `div` 2) bs


checkLength :: Int -> [a] -> [a]
checkLength n xs =
  if length xs /= n
    then error "Incorrect Length"
    else xs


checkLengthMultiple :: Int -> [a] -> [a]
checkLengthMultiple n xs =
  if length xs `mod` n /= 0
    then error "Incorrect Length Multiple"
    else xs


------------------------------------------------------------------
--                 Bit Conversions Functions                    --
------------------------------------------------------------------


--nothing = bitsToString $ stringToBits

stringToBits :: String -> Bits
stringToBits s = concatMap (pad 8 . intToBits . charToInt) s

charToInt :: Char -> Int
charToInt c | n >= 48 && n <= 57 = n - 48
            | n >= 97 && n <= 122 = n - 87
            | c == ' ' = 36
            | c == ',' = 37
            | otherwise = error "Not valid"
              where n = ord (toLower c)

intToBits :: Int -> Bits
intToBits 0 = []
intToBits n = intToBits (div n 2) ++ [(mod n 2)]

pad :: Int -> Bits -> Bits
pad n xs = replicate (n - length xs) 0 ++ xs

bitsToString :: Bits -> String
bitsToString bs = map (intsToChar . bitsToInt) $ group 8 bs

bitsToInt :: Bits -> Int
bitsToInt [] = 0
bitsToInt (b:bs) = b * 2^(length bs) + bitsToInt bs

intsToChar :: Int -> Char
intsToChar n | n >= 0 && n<= 9 = head $ show n
             | n >= 10 && n<= 35 = chr $ n + 87
             | n == 36 = ' '
             | n == 37 = ','
             | otherwise = chr n --error "Not valid"

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = as : group n bs 
  where (as, bs) = splitAt n xs


leftShift :: Int -> Bits -> Bits
leftShift n xs = take (length xs) . drop n $ cycle xs


------------------------------------------------------------------
--                           Tables                             --
------------------------------------------------------------------


sbox1 :: [[Bits]]
sbox1 = map (map (pad 4 . intToBits)) [
  [14, 04, 13, 01, 02, 15, 11, 08, 03, 10, 06, 12, 05, 09, 00, 07],
  [00, 15, 07, 04, 14, 02, 13, 01, 10, 06, 12, 11, 09, 05, 03, 08],
  [04, 01, 14, 08, 13, 06, 02, 11, 15, 12, 09, 07, 03, 10, 05, 00],
  [15, 12, 08, 02, 04, 09, 01, 07, 05, 11, 03, 14, 10, 00, 06, 13]]

sbox2 :: [[Bits]]
sbox2 = map (map (pad 4 . intToBits)) [
  [15, 01, 08, 14, 06, 11, 03, 04, 09, 07, 02, 13, 12, 00, 05, 10],
  [03, 13, 04, 07, 15, 02, 08, 14, 12, 00, 01, 10, 06, 09, 11, 05],
  [00, 14, 07, 11, 10, 04, 13, 01, 05, 08, 12, 06, 09, 03, 02, 15],
  [13, 08, 10, 01, 03, 15, 04, 02, 11, 06, 07, 12, 00, 05, 14, 09]]

sbox3 :: [[Bits]]
sbox3 = map (map (pad 4 . intToBits)) [
  [10, 00, 09, 14, 06, 03, 15, 05, 01, 13, 12, 07, 11, 04, 02, 08],
  [13, 07, 00, 09, 03, 04, 06, 10, 02, 08, 05, 14, 12, 11, 15, 01],
  [13, 06, 04, 09, 08, 15, 03, 00, 11, 01, 02, 12, 05, 10, 14, 07],
  [01, 10, 13, 00, 06, 09, 08, 07, 04, 15, 14, 03, 11, 05, 02, 12]]

sbox4 :: [[Bits]]
sbox4 = map (map (pad 4 . intToBits)) [
  [07, 13, 14, 03, 00, 06, 09, 10, 01, 02, 08, 05, 11, 12, 04, 15],
  [13, 08, 11, 05, 06, 15, 00, 03, 04, 07, 02, 12, 01, 10, 14, 09],
  [10, 06, 09, 00, 12, 11, 07, 13, 15, 01, 03, 14, 05, 02, 08, 04],
  [03, 15, 00, 06, 10, 01, 13, 08, 09, 04, 05, 11, 12, 07, 02, 14]]

sbox5 :: [[Bits]]
sbox5 = map (map (pad 4 . intToBits)) [
  [02, 12, 04, 01, 07, 10, 11, 06, 08, 05, 03, 15, 13, 00, 14, 09],
  [14, 11, 02, 12, 04, 07, 13, 01, 05, 00, 15, 10, 03, 09, 08, 06],
  [04, 02, 01, 11, 10, 13, 07, 08, 15, 09, 12, 05, 06, 03, 00, 14],
  [11, 08, 12, 07, 01, 14, 02, 13, 06, 15, 00, 09, 10, 04, 05, 03]]

sbox6 :: [[Bits]]
sbox6 = map (map (pad 4 . intToBits)) [
  [12, 01, 10, 15, 09, 02, 06, 08, 00, 13, 03, 04, 14, 07, 05, 11],
  [10, 15, 04, 02, 07, 12, 09, 05, 06, 01, 13, 14, 00, 11, 03, 08],
  [09, 14, 15, 05, 02, 08, 12, 03, 07, 00, 04, 10, 01, 13, 11, 06],
  [04, 03, 02, 12, 09, 05, 15, 10, 11, 14, 01, 07, 06, 00, 08, 13]]

sbox7 :: [[Bits]]
sbox7 = map (map (pad 4 . intToBits)) [
  [04, 11, 02, 14, 15, 00, 08, 13, 03, 12, 09, 07, 05, 10, 06, 01],
  [13, 00, 11, 07, 04, 09, 01, 10, 14, 03, 05, 12, 02, 15, 08, 06],
  [01, 04, 11, 13, 12, 03, 07, 14, 10, 15, 06, 08, 00, 05, 09, 02],
  [06, 11, 13, 08, 01, 04, 10, 07, 09, 05, 00, 15, 14, 02, 03, 12]]

sbox8 :: [[Bits]]
sbox8 = map (map (pad 4 . intToBits)) [
  [13, 02, 08, 04, 06, 15, 11, 01, 10, 09, 03, 14, 05, 00, 12, 07],
  [01, 15, 13, 08, 10, 03, 07, 04, 12, 05, 06, 11, 00, 14, 09, 02],
  [07, 11, 04, 01, 09, 12, 14, 02, 00, 06, 10, 13, 15, 03, 05, 08],
  [02, 01, 14, 07, 04, 10, 08, 13, 15, 12, 09, 00, 03, 05, 06, 11]]


applyTable :: [Int] -> Bits -> Bits
applyTable table bs = map (\i -> bs !! (i-1)) table

permutationTable :: [Int]
permutationTable = [
  16, 07, 20, 21, 29, 12, 28, 17,
  01, 15, 23, 26, 05, 18, 31, 10,
  02, 08, 24, 14, 32, 27, 03, 09,
  19, 13, 30, 06, 22, 11, 04, 25]

expansionTable :: [Int]
expansionTable = [
  32, 01, 02, 03, 04, 05, 04, 05,
  06, 07, 08, 09, 08, 09, 10, 11,
  12, 13, 12, 13, 14, 15, 16, 17,
  16, 17, 18, 19, 20, 21, 20, 21,
  22, 23, 24, 25, 24, 25, 26, 27,
  28, 29, 28, 29, 30, 31, 32, 01]


initialPermutationTable :: [Int]
initialPermutationTable = [
  58, 50, 42, 34, 26, 18, 10, 02,
  60, 52, 44, 36, 28, 20, 12, 04,
  62, 54, 46, 38, 30, 22, 14, 06,
  64, 56, 48, 40, 32, 24, 16, 08,
  57, 49, 41, 33, 25, 17, 09, 01,
  59, 51, 43, 35, 27, 19, 11, 03,
  61, 53, 45, 37, 29, 21, 13, 05,
  63, 55, 47, 39, 31, 23, 15, 07]

finalPermutationTable :: [Int]
finalPermutationTable = [
  40, 08, 48, 16, 56, 24, 64, 32,
  39, 07, 47, 15, 55, 23, 63, 31,
  38, 06, 46, 14, 54, 22, 62, 30,
  37, 05, 45, 13, 53, 21, 61, 29,
  36, 04, 44, 12, 52, 20, 60, 28,
  35, 03, 43, 11, 51, 19, 59, 27,
  34, 02, 42, 10, 50, 18, 58, 26,
  33, 01, 41, 09, 49, 17, 57, 25]

initialKeyPermutationTable :: [Int]
initialKeyPermutationTable = [
  57, 49, 41, 33, 25, 17, 09, 01,
  58, 50, 42, 34, 26, 18, 10, 02,
  59, 51, 43, 35, 27, 19, 11, 03,
  60, 52, 44, 36, 63, 55, 47, 39,
  31, 23, 15, 07, 62, 54, 46, 38,
  30, 22, 14, 06, 61, 53, 45, 37,
  29, 21, 13, 05, 28, 20, 12, 04]

roundKeyPermuationTable :: [Int]
roundKeyPermuationTable = [
  14, 17, 11, 24, 01, 05,
  03, 28, 15, 06, 21, 10,
  23, 19, 12, 04, 26, 08,
  16, 07, 27, 20, 13, 02,
  41, 52, 31, 37, 47, 55,
  30, 40, 51, 45, 33, 48,
  44, 49, 39, 56, 34, 53,
  46, 42, 50, 36, 29, 32]