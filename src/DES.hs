module DES where

import Binary
import Debug.Trace
import Test.HUnit

type Key = Binary
type Nonce = Binary




------------------------------------------------------------------
--                            Tests                             --
------------------------------------------------------------------

testDES :: Test
testDES = TestList [
  let key = map (\c -> c=='1') "0111010100101000011110000011100101110100100100111100101101110000"
      plaintext = map (\c -> c=='1') "0001000100100010001100110100010001010101011001100111011110001000"
      target =    map (\c -> c=='1') "1011010100100001100111101110100000011010101001110100100110011101"
  in TestCase $ assertEqual "des failed 1" target (des key plaintext),
  let key =       map (\c -> c=='1') "0111010100101000011110000011100101110100100100111100101101110000"
      plaintext = map (\c -> c=='1') "0000000000000000000000000000000000000000000000000000000000000000"
      target =    map (\c -> c=='1') "0101011001110010000111100100000010100110100110010001010010101110"
  in TestCase $ assertEqual "des failed 2" target (des key plaintext),
  let key =       map (\c -> c=='1') "0111010100101000011110000011100101110100100100111100101101110000"
      plaintext = map (\c -> c=='1') "0000000000000000000000000000000000000000000000000000000000000001"
      target =    map (\c -> c=='1') "0101010001110010010000000100000100101100110110000011101000000110"
  in TestCase $ assertEqual "des failed 3" target (des key plaintext),
  let key =       map (\c -> c=='1') "1101111101100001011010001110100000111010101101111001000100011110"
      plaintext = map (\c -> c=='1') "1000111001100000110111100000101010010111111001001110111001000001"
      target =    map (\c -> c=='1') "0000000101000101001001001101100100111111010010000010011111011001"
  in TestCase $ assertEqual "des failed 4" target (des key plaintext),
  let key =       map (\c -> c=='1') "1011110110001010000100011000110101101010001111101010000110000010"
      plaintext = map (\c -> c=='1') "1000110011110110111110000011101001111100100111011000000110101111"
      target =    map (\c -> c=='1') "1111110110000100101110111011000000101001100011000011011101000000"
  in TestCase $ assertEqual "des failed 5" target (des key plaintext),
  let key =       map (\c -> c=='1') "0100101101000111100010010001100001101000011100101000100000100000"
      plaintext = map (\c -> c=='1') "0110111100011010110000111011111001010101110001001001110110101011"
      target =    map (\c -> c=='1') "1100101110101111011001110010101110011010010010010110110100001001"
  in TestCase $ assertEqual "des failed 6" target (des key plaintext),
  let key =       map (\c -> c=='1') "0100101101000111100010010001100001101000011100101000100000100000"
      plaintext = map (\c -> c=='1') "0000000000000000000000000000000000000000000000000000000000000000"
      target =    map (\c -> c=='1') "1100010010110011011111000001110011110111000110100011100101011011"
  in TestCase $ assertEqual "des failed 7" target (des key plaintext)]

tests :: Test
tests = TestList [
  testDES]

mainTest :: IO ()
mainTest = do
  runTestTT tests
  putStrLn "Done"


------------------------------------------------------------------
--                    Encryption Functions                      --
------------------------------------------------------------------

 
encrypt :: Key -> Nonce -> Binary -> Binary
encrypt key nonce = ctrMode des key nonce . padPKCS7

decrypt :: Key -> Nonce -> Binary -> Binary
decrypt key nonce = depadPKCS7 . ctrMode des key nonce


------------------------------------------------------------------
--                    Counter Mode Functions                    --
------------------------------------------------------------------


ctrMode :: (Key -> Binary -> Binary) -> Key -> Nonce -> Binary -> Binary
ctrMode cipher key nonce binary =
  concat [ctrBlock cipher key nonce counter block | (counter, block) <- zip counters blocks]
    where counters = [(pad 32 . intToBinary) n | n <- [1..]]
          blocks = group 64 binary
          len = length blocks

ctrBlock :: (Key -> Binary -> Binary) -> Key -> Nonce -> Binary -> Binary -> Binary
ctrBlock cipher key nonce counter binary = binary `xor` (cipher key (nonce ++ counter))



------------------------------------------------------------------
--                        DES Functions                         --
------------------------------------------------------------------



des :: Key -> Binary -> Binary
des key bits = (finalPermutation . crypt . initialPermutation) bits
  where crypt = uncurry (++) . applyKeys keys . halves
        keys = keySchedule key

applyKeys :: [Key] -> (Binary, Binary) -> (Binary, Binary)
applyKeys [] (left, right) = (right, left)
applyKeys (key:keys) (left, right) =
  applyKeys keys $ feistel key (left, right)





feistel :: Key -> (Binary, Binary) -> (Binary, Binary)
feistel key (left, right) = (right, encrypted)
  where encrypted = xor (f key right) left


----------------------------------------------------------------
--                        PKCS7 Padding                       --
----------------------------------------------------------------

padPKCS7 :: Binary -> Binary
padPKCS7 binary
  | l `mod` 8 /= 0 = error "Has to be in bytes"
  | l `mod` 64 == 0 = binary ++ (concat $ replicate 8 (pad 8 $ intToBinary $ 8))
  | otherwise = binary ++ padding
    where l = length binary
          n = ((64 - (l `mod` 64)) `mod` 64) `div` 8
          x = (pad 8 . intToBinary) n
          padding = concat $ replicate n x

depadPKCS7 :: Binary -> Binary
depadPKCS7 binary
  | l `mod` 8 /= 0 = error "Has to be in bytes"
  | otherwise = (reverse . drop (8*b) . reverse) binary
    where l = length binary
          b = binaryToInt $ (reverse . take 8 . reverse) binary



----------------------------------------------------------------
--                          F Function                        --
----------------------------------------------------------------


f :: Key -> Binary -> Binary
f key = permutation . sboxes . (xor key) . expansionFunction



sboxes :: Binary -> Binary
sboxes = concat . zipWith ($) boxes . group 6
  where boxes = [sbox1, sbox2, sbox3, sbox4,
                 sbox5, sbox6, sbox7, sbox8]



----------------------------------------------------------------
--                        Key Schedule                        --
----------------------------------------------------------------


keySchedule :: Key -> [Key]
keySchedule key = subKeys shifts (left, right)
  where shifts = [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]
        (left, right) = (halves . permutedChoice1) key

subKeys :: [Int] -> (Key, Key) -> [Key]
subKeys [] _ = []
subKeys (shift:shifts) (left, right) =
  permutedChoice2 (uncurry (++) subKey) : subKeys shifts subKey
  where subKey = (left', right')
        left' = leftShift shift left
        right' = leftShift shift right

----------------------------------------------------------------
--                           S-Boxes                          --
----------------------------------------------------------------



-- 6 bit input, 4 bit output
applySbox :: [[Int]] -> Binary -> Binary
applySbox sbox binary = (pad 4 . intToBinary) $ sbox !! row !! column
  where row = binaryToInt $ getIndexes [0,5] binary
        column = binaryToInt $ getIndexes [1,2,3,4] binary


sbox1 :: Binary -> Binary
sbox1 = applySbox [
  [14, 04, 13, 01, 02, 15, 11, 08, 03, 10, 06, 12, 05, 09, 00, 07],
  [00, 15, 07, 04, 14, 02, 13, 01, 10, 06, 12, 11, 09, 05, 03, 08],
  [04, 01, 14, 08, 13, 06, 02, 11, 15, 12, 09, 07, 03, 10, 05, 00],
  [15, 12, 08, 02, 04, 09, 01, 07, 05, 11, 03, 14, 10, 00, 06, 13]]

sbox2 :: Binary -> Binary
sbox2 = applySbox [
  [15, 01, 08, 14, 06, 11, 03, 04, 09, 07, 02, 13, 12, 00, 05, 10],
  [03, 13, 04, 07, 15, 02, 08, 14, 12, 00, 01, 10, 06, 09, 11, 05],
  [00, 14, 07, 11, 10, 04, 13, 01, 05, 08, 12, 06, 09, 03, 02, 15],
  [13, 08, 10, 01, 03, 15, 04, 02, 11, 06, 07, 12, 00, 05, 14, 09]]

sbox3 :: Binary -> Binary
sbox3 = applySbox [
  [10, 00, 09, 14, 06, 03, 15, 05, 01, 13, 12, 07, 11, 04, 02, 08],
  [13, 07, 00, 09, 03, 04, 06, 10, 02, 08, 05, 14, 12, 11, 15, 01],
  [13, 06, 04, 09, 08, 15, 03, 00, 11, 01, 02, 12, 05, 10, 14, 07],
  [01, 10, 13, 00, 06, 09, 08, 07, 04, 15, 14, 03, 11, 05, 02, 12]]

sbox4 :: Binary -> Binary
sbox4 = applySbox [
  [07, 13, 14, 03, 00, 06, 09, 10, 01, 02, 08, 05, 11, 12, 04, 15],
  [13, 08, 11, 05, 06, 15, 00, 03, 04, 07, 02, 12, 01, 10, 14, 09],
  [10, 06, 09, 00, 12, 11, 07, 13, 15, 01, 03, 14, 05, 02, 08, 04],
  [03, 15, 00, 06, 10, 01, 13, 08, 09, 04, 05, 11, 12, 07, 02, 14]]

sbox5 :: Binary -> Binary
sbox5 = applySbox [
  [02, 12, 04, 01, 07, 10, 11, 06, 08, 05, 03, 15, 13, 00, 14, 09],
  [14, 11, 02, 12, 04, 07, 13, 01, 05, 00, 15, 10, 03, 09, 08, 06],
  [04, 02, 01, 11, 10, 13, 07, 08, 15, 09, 12, 05, 06, 03, 00, 14],
  [11, 08, 12, 07, 01, 14, 02, 13, 06, 15, 00, 09, 10, 04, 05, 03]]

sbox6 :: Binary -> Binary
sbox6 = applySbox [
  [12, 01, 10, 15, 09, 02, 06, 08, 00, 13, 03, 04, 14, 07, 05, 11],
  [10, 15, 04, 02, 07, 12, 09, 05, 06, 01, 13, 14, 00, 11, 03, 08],
  [09, 14, 15, 05, 02, 08, 12, 03, 07, 00, 04, 10, 01, 13, 11, 06],
  [04, 03, 02, 12, 09, 05, 15, 10, 11, 14, 01, 07, 06, 00, 08, 13]]

sbox7 :: Binary -> Binary
sbox7 = applySbox [
  [04, 11, 02, 14, 15, 00, 08, 13, 03, 12, 09, 07, 05, 10, 06, 01],
  [13, 00, 11, 07, 04, 09, 01, 10, 14, 03, 05, 12, 02, 15, 08, 06],
  [01, 04, 11, 13, 12, 03, 07, 14, 10, 15, 06, 08, 00, 05, 09, 02],
  [06, 11, 13, 08, 01, 04, 10, 07, 09, 05, 00, 15, 14, 02, 03, 12]]

sbox8 :: Binary -> Binary
sbox8 = applySbox [
  [13, 02, 08, 04, 06, 15, 11, 01, 10, 09, 03, 14, 05, 00, 12, 07],
  [01, 15, 13, 08, 10, 03, 07, 04, 12, 05, 06, 11, 00, 14, 09, 02],
  [07, 11, 04, 01, 09, 12, 14, 02, 00, 06, 10, 13, 15, 03, 05, 08],
  [02, 01, 14, 07, 04, 10, 08, 13, 15, 12, 09, 00, 03, 05, 06, 11]]



----------------------------------------------------------------
--                           Tables                           --
----------------------------------------------------------------


applyTable :: [Int] -> Binary -> Binary
applyTable table binary = map (\i -> binary !! (i-1)) table

-- Permutation (P)
permutation :: Binary -> Binary
permutation = applyTable [
  16, 07, 20, 21, 29, 12, 28, 17,
  01, 15, 23, 26, 05, 18, 31, 10,
  02, 08, 24, 14, 32, 27, 03, 09,
  19, 13, 30, 06, 22, 11, 04, 25]

-- Expansion function (E)
expansionFunction :: Binary -> Binary
expansionFunction = applyTable [
  32, 01, 02, 03, 04, 05, 04, 05,
  06, 07, 08, 09, 08, 09, 10, 11,
  12, 13, 12, 13, 14, 15, 16, 17,
  16, 17, 18, 19, 20, 21, 20, 21,
  22, 23, 24, 25, 24, 25, 26, 27,
  28, 29, 28, 29, 30, 31, 32, 01]


-- Initial permutation (IP)
initialPermutation :: Binary -> Binary
initialPermutation = applyTable [
  58, 50, 42, 34, 26, 18, 10, 02,
  60, 52, 44, 36, 28, 20, 12, 04,
  62, 54, 46, 38, 30, 22, 14, 06,
  64, 56, 48, 40, 32, 24, 16, 08,
  57, 49, 41, 33, 25, 17, 09, 01,
  59, 51, 43, 35, 27, 19, 11, 03,
  61, 53, 45, 37, 29, 21, 13, 05,
  63, 55, 47, 39, 31, 23, 15, 07]

-- Final permutation (IP−1)
finalPermutation :: Binary -> Binary
finalPermutation = applyTable [
  40, 08, 48, 16, 56, 24, 64, 32,
  39, 07, 47, 15, 55, 23, 63, 31,
  38, 06, 46, 14, 54, 22, 62, 30,
  37, 05, 45, 13, 53, 21, 61, 29,
  36, 04, 44, 12, 52, 20, 60, 28,
  35, 03, 43, 11, 51, 19, 59, 27,
  34, 02, 42, 10, 50, 18, 58, 26,
  33, 01, 41, 09, 49, 17, 57, 25]

-- Permuted choice 1 (PC-1)
permutedChoice1 :: Binary -> Binary
permutedChoice1 = applyTable [
  57, 49, 41, 33, 25, 17, 09, 01,
  58, 50, 42, 34, 26, 18, 10, 02,
  59, 51, 43, 35, 27, 19, 11, 03,
  60, 52, 44, 36, 63, 55, 47, 39,
  31, 23, 15, 07, 62, 54, 46, 38,
  30, 22, 14, 06, 61, 53, 45, 37,
  29, 21, 13, 05, 28, 20, 12, 04]

-- Permuted choice 2 (PC-2)
permutedChoice2 :: Binary -> Binary
permutedChoice2 = applyTable [
  14, 17, 11, 24, 01, 05,
  03, 28, 15, 06, 21, 10,
  23, 19, 12, 04, 26, 08,
  16, 07, 27, 20, 13, 02,
  41, 52, 31, 37, 47, 55,
  30, 40, 51, 45, 33, 48,
  44, 49, 39, 56, 34, 53,
  46, 42, 50, 36, 29, 32]

