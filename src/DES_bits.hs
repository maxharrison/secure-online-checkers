module DES where

import Debug.Trace
import Data.Word
import Data.Bits 

import Data.Char

import Numeric (showIntAtBase)

type Key = Word

showBinary b = showIntAtBase 2 intToDigit b ""

key1 :: Key
key1 = 0b0111010100101000011110000011100101110100100100111100101101110000


------------------------------------------------------------------
--                       Helper Functions                       --
------------------------------------------------------------------

halves :: Bits a => Int -> a -> (a, a)
halves size bits = (bits `shiftR` half_size, bits .&. mask half_size)
  where half_size = size `div` 2

unhalves :: Bits a => Int -> (a, a) -> a
unhalves size (left, right) = (right .|. (left `shiftL` half_size)) .&. mask size
    where half_size = size `div` 2

group :: Bits a => Int -> a -> [a]
group size x
  | popCount x == 0 = []
  | otherwise =
    let y = x .&. mask size
        xs = group size (x `shiftR` size)
    in xs ++ [y]

ungroup :: Bits a => Int -> [a] -> a
ungroup _ [] = zero
ungroup size (x:xs) = (x `shiftL` n) .|. ungroup size xs
  where n = size * length xs

cShiftL:: Bits a => Int -> Int -> a -> a
cShiftL size n x = (x `shiftL` n) .&. mask size .|. x `shiftR` (size - n)

mask :: Bits a => Int -> a
mask n = foldl setBit zeroBits [0..n-1]

one :: Bits a => a
one = bit 0

zero :: Bits a => a
zero = zeroBits

getBitBETTER :: Bits a => a -> Int -> a
getBitBETTER x n = if testBit x n then one else zero

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

------------------------------------------------------------------




halves64 :: Word -> (Word, Word)
halves64 word = (word `shiftR` 32, word .&. 0xFFFFFFFF)
unhalves64 :: (Word, Word) -> Word
unhalves64 (left, right) = (right .|. (left `shiftL` 32)) .&. 0xFFFFFFFFFFFFFFFF

rotateL28 :: Int -> Word -> Word
rotateL28 n x = ((x `shiftL` n) .&. 0xFFFFFFF)  .|. (x `shiftR` (28 - n))

halves56 :: Word -> (Word, Word)
halves56 word = (word `shiftR` 28, word .&. 0xFFFFFFF)

unhalves56 :: (Word, Word) -> Word
unhalves56 (left, right) = (right .|. (left `shiftL` 28)) .&. 0xFFFFFFFFFFFFFF


{- plaintextTest = map (\i -> if i==1 then True else False)
  [0, 0, 0, 1, 0, 0, 0, 1,
   0, 0, 1, 0, 0, 0, 1, 0,
   0, 0, 1, 1, 0, 0, 1, 1,
   0, 1, 0, 0, 0, 1, 0, 0,
   0, 1, 0, 1, 0, 1, 0, 1,
   0, 1, 1, 0, 0, 1, 1, 0,
   0, 1, 1, 1, 0, 1, 1, 1,
   1, 0, 0, 0, 1, 0, 0, 0]

key = map (\i -> if i==1 then True else False)
  [0, 1, 1, 1, 0, 1, 0, 1,
   0, 0, 1, 0, 1, 0, 0, 0,
   0, 1, 1, 1, 1, 0, 0, 0,
   0, 0, 1, 1, 1, 0, 0, 1,
   0, 1, 1, 1, 0, 1, 0, 0,
   1, 0, 0, 1, 0, 0, 1, 1,
   1, 1, 0, 0, 1, 0, 1, 1,
   0, 1, 1, 1, 0, 0, 0, 0]

targetTest = map (\i -> if i==1 then True else False)
  [1, 0, 1, 1, 0, 1, 0, 1,
   0, 0, 1, 0, 0, 0, 0, 1,
   1, 0, 0, 1, 1, 1, 1, 0,
   1, 1, 1, 0, 1, 0, 0, 0,
   0, 0, 0, 1, 1, 0, 1, 0,
   1, 0, 1, 0, 0, 1, 1, 1,
   0, 1, 0, 0, 1, 0, 0, 1,
   1, 0, 0, 1, 1, 1, 0, 1]


main = do
  let plaintext = plaintextTest
  let ciphertext = encrypt key plaintext
  let decrypted = decrypt key ciphertext
  putStrLn $ "plaintext:    " ++ showBinary plaintext
  putStrLn $ "ciphertext:   " ++ showBinary ciphertext
  putStrLn $ "decrypted:    " ++ showBinary decrypted
  print $ ciphertext == targetTest

-}


------------------------------------------------------------------
--                        Main Functions                        --
------------------------------------------------------------------


--encrypt :: Key -> Nonce -> Word -> Word
--encrypt key nonce binary = (ctrMode des key nonce . padPKCS7) word

--decrypt :: Key -> Nonce -> Word -> Word
--decrypt key nonce word = (depadPKCS7 . ctrMode des key nonce) word


----------------------------------------------------------------
--                        PKCS7 Padding                       --
----------------------------------------------------------------




padPKCS7 :: Int -> [Word8] -> [Word8]
padPKCS7 block_len bytes = bytes ++ padding
  where padding_len = ((block_len `div` 8) - (length bytes `mod` (block_len `div` 8)))
        padding = replicate padding_len (fromIntegral padding_len)


unpadPKCS7 :: [Word8] -> [Word8]
unpadPKCS7 bytes = take len bytes
  where bytes_len = length bytes
        padding_len = fromIntegral $ last bytes
        len = bytes_len - padding_len



------------------------------------------------------------------
--                      CTR - Counter Mode                      --
------------------------------------------------------------------

type Nonce = Word32 -- MAYBE SELECT A LENGTH
type Counter = Word32


--ctrMode :: (Key -> Word -> Word) -> Key -> Nonce -> Word -> Word
--ctrMode cipher key nonce binary =
--  concat [ctrBlock cipher key nonce counter block | (counter, block) <- zip counters blocks]
--    where counters = [(pad 32 . intToBinary) n | n <- [0..len]]
--          blocks = group 64 binary
--          len = length blocks

ctrBlock :: (Key -> Word -> Word) -> Key -> Nonce -> Counter -> [Word8] -> [Word8]
ctrBlock cipher key nonce counter bytes =
  group 8 $ (block_key `xor` block)
    where counter_block = fromIntegral $ ungroup 32 [nonce, counter]
          block_key = fromIntegral $ cipher key counter_block
          block = fromIntegral $ ungroup 64 bytes


------------------------------------------------------------------
--                        DES Functions                        --
------------------------------------------------------------------


des :: Key -> Word -> Word
des key = (finalPermutation . crypt . initialPermutation)
  where crypt = unhalves64 . applyKeys keys . halves64
        keys = keySchedule key

applyKeys :: [Key] -> (Word, Word) -> (Word, Word)
applyKeys [] (left, right) = (right, left)
applyKeys (key:keys) (left, right) =
  applyKeys keys $ feistel key (left, right)

feistel :: Key -> (Word, Word) -> (Word, Word)
feistel key (left, right) = (right, encrypted)
  where encrypted = (f key right) `xor` left



----------------------------------------------------------------
--                          F Function                        --
----------------------------------------------------------------


f :: Key -> Word -> Word
f key = permutation . sboxes . (xor key) . expansionFunction

sboxes :: Word -> Word
sboxes = ungroup 6 . zipWith ($) boxes . group 6
  where boxes = [sbox1, sbox2, sbox3, sbox4,
                 sbox5, sbox6, sbox7, sbox8]


----------------------------------------------------------------
--                        Key Schedule                        --
----------------------------------------------------------------


keySchedule :: Key -> [Key]
keySchedule = subKeys shifts . permutedChoice1
  where shifts = [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]

subKeys :: [Int] -> Key -> [Key]
subKeys [] _ = []
subKeys (shift:shifts) subKey =
  permutedChoice2 subKey' : subKeys shifts subKey'
  where subKey' = (rotateHalves shift subKey)

rotateHalves :: Int -> Word -> Word
rotateHalves n word =
  let (left, right) = halves56 word
      left' = rotateL28 n left
      right' = rotateL28 n right
  in unhalves56 (left', right')


----------------------------------------------------------------
--                           S-Boxes                          --
----------------------------------------------------------------


-- 6 bit input, 4 bit output
applySbox :: [[Word]] -> Word -> Word
applySbox sbox word = sbox !! row !! column
  where row = fromIntegral $ getNumber word [5,0]
        column = fromIntegral $ getNumber word [4,3,2,1]

getNumber :: Word -> [Int] -> Word
getNumber _ [] = 0b0
getNumber word (x:xs) = shiftL (getBit x word) (length xs) .|. getNumber word xs

getBit :: Int -> Word -> Word
getBit k n  = (shiftR n k) .&. 1

sbox1 :: Word -> Word
sbox1 = applySbox [
  [14, 04, 13, 01, 02, 15, 11, 08, 03, 10, 06, 12, 05, 09, 00, 07],
  [00, 15, 07, 04, 14, 02, 13, 01, 10, 06, 12, 11, 09, 05, 03, 08],
  [04, 01, 14, 08, 13, 06, 02, 11, 15, 12, 09, 07, 03, 10, 05, 00],
  [15, 12, 08, 02, 04, 09, 01, 07, 05, 11, 03, 14, 10, 00, 06, 13]]

sbox2 :: Word -> Word
sbox2 = applySbox [
  [15, 01, 08, 14, 06, 11, 03, 04, 09, 07, 02, 13, 12, 00, 05, 10],
  [03, 13, 04, 07, 15, 02, 08, 14, 12, 00, 01, 10, 06, 09, 11, 05],
  [00, 14, 07, 11, 10, 04, 13, 01, 05, 08, 12, 06, 09, 03, 02, 15],
  [13, 08, 10, 01, 03, 15, 04, 02, 11, 06, 07, 12, 00, 05, 14, 09]]

sbox3 :: Word -> Word
sbox3 = applySbox [
  [10, 00, 09, 14, 06, 03, 15, 05, 01, 13, 12, 07, 11, 04, 02, 08],
  [13, 07, 00, 09, 03, 04, 06, 10, 02, 08, 05, 14, 12, 11, 15, 01],
  [13, 06, 04, 09, 08, 15, 03, 00, 11, 01, 02, 12, 05, 10, 14, 07],
  [01, 10, 13, 00, 06, 09, 08, 07, 04, 15, 14, 03, 11, 05, 02, 12]]

sbox4 :: Word -> Word
sbox4 = applySbox [
  [07, 13, 14, 03, 00, 06, 09, 10, 01, 02, 08, 05, 11, 12, 04, 15],
  [13, 08, 11, 05, 06, 15, 00, 03, 04, 07, 02, 12, 01, 10, 14, 09],
  [10, 06, 09, 00, 12, 11, 07, 13, 15, 01, 03, 14, 05, 02, 08, 04],
  [03, 15, 00, 06, 10, 01, 13, 08, 09, 04, 05, 11, 12, 07, 02, 14]]

sbox5 :: Word -> Word
sbox5 = applySbox [
  [02, 12, 04, 01, 07, 10, 11, 06, 08, 05, 03, 15, 13, 00, 14, 09],
  [14, 11, 02, 12, 04, 07, 13, 01, 05, 00, 15, 10, 03, 09, 08, 06],
  [04, 02, 01, 11, 10, 13, 07, 08, 15, 09, 12, 05, 06, 03, 00, 14],
  [11, 08, 12, 07, 01, 14, 02, 13, 06, 15, 00, 09, 10, 04, 05, 03]]

sbox6 :: Word -> Word
sbox6 = applySbox [
  [12, 01, 10, 15, 09, 02, 06, 08, 00, 13, 03, 04, 14, 07, 05, 11],
  [10, 15, 04, 02, 07, 12, 09, 05, 06, 01, 13, 14, 00, 11, 03, 08],
  [09, 14, 15, 05, 02, 08, 12, 03, 07, 00, 04, 10, 01, 13, 11, 06],
  [04, 03, 02, 12, 09, 05, 15, 10, 11, 14, 01, 07, 06, 00, 08, 13]]

sbox7 :: Word -> Word
sbox7 = applySbox [
  [04, 11, 02, 14, 15, 00, 08, 13, 03, 12, 09, 07, 05, 10, 06, 01],
  [13, 00, 11, 07, 04, 09, 01, 10, 14, 03, 05, 12, 02, 15, 08, 06],
  [01, 04, 11, 13, 12, 03, 07, 14, 10, 15, 06, 08, 00, 05, 09, 02],
  [06, 11, 13, 08, 01, 04, 10, 07, 09, 05, 00, 15, 14, 02, 03, 12]]

sbox8 :: Word -> Word
sbox8 = applySbox [
  [13, 02, 08, 04, 06, 15, 11, 01, 10, 09, 03, 14, 05, 00, 12, 07],
  [01, 15, 13, 08, 10, 03, 07, 04, 12, 05, 06, 11, 00, 14, 09, 02],
  [07, 11, 04, 01, 09, 12, 14, 02, 00, 06, 10, 13, 15, 03, 05, 08],
  [02, 01, 14, 07, 04, 10, 08, 13, 15, 12, 09, 00, 03, 05, 06, 11]]



----------------------------------------------------------------
--                           Tables                           --
----------------------------------------------------------------


applyTable1 :: Int -> Int -> [Int] -> Word -> Word
applyTable1 input_len output_len table word = build (zip [0..] table) word
    where build [] _ = 0b1
          build ((output_pos, pos):xs) word =
            let input_pos = input_len - (pos)
                input_bit = (word `shiftR` input_pos) .&. 0b1
                output_bit = input_bit `shiftL` ((output_len - 1) - output_pos)
            in output_bit .|. build xs word


applyTable :: Bits a => Int -> [Int] -> a -> a
applyTable _ [] _ = zero
applyTable input_len (pos:xs) bits =
  let input_pos = input_len - pos
      input_bit = getBitBETTER bits input_pos
      output_pos = length xs
      output_bit = input_bit `shiftL` output_pos
  in output_bit .|. applyTable input_len xs bits




-- Permutation (P)
-- 32 bit input
-- 32 bit output
permutation :: Bits a => a -> a
permutation = applyTable 32 [
  16, 07, 20, 21, 29, 12, 28, 17,
  01, 15, 23, 26, 05, 18, 31, 10,
  02, 08, 24, 14, 32, 27, 03, 09,
  19, 13, 30, 06, 22, 11, 04, 25]

-- Expansion function (E)
-- 32 bit input
-- 48 bit output
expansionFunction :: Bits a => a -> a
expansionFunction = applyTable 32 [
  32, 01, 02, 03, 04, 05, 04, 05,
  06, 07, 08, 09, 08, 09, 10, 11,
  12, 13, 12, 13, 14, 15, 16, 17,
  16, 17, 18, 19, 20, 21, 20, 21,
  22, 23, 24, 25, 24, 25, 26, 27,
  28, 29, 28, 29, 30, 31, 32, 01]

-- Initial permutation (IP)
-- 64 bit input
-- 64 bit output
initialPermutation :: Bits a => a -> a
initialPermutation = applyTable 64 [
  58, 50, 42, 34, 26, 18, 10, 02,
  60, 52, 44, 36, 28, 20, 12, 04,
  62, 54, 46, 38, 30, 22, 14, 06,
  64, 56, 48, 40, 32, 24, 16, 08,
  57, 49, 41, 33, 25, 17, 09, 01,
  59, 51, 43, 35, 27, 19, 11, 03,
  61, 53, 45, 37, 29, 21, 13, 05,
  63, 55, 47, 39, 31, 23, 15, 07]

-- Final permutation (IP−1)
-- 64 bit input
-- 64 bit output
finalPermutation :: Bits a => a -> a
finalPermutation = applyTable 64 [
  40, 08, 48, 16, 56, 24, 64, 32,
  39, 07, 47, 15, 55, 23, 63, 31,
  38, 06, 46, 14, 54, 22, 62, 30,
  37, 05, 45, 13, 53, 21, 61, 29,
  36, 04, 44, 12, 52, 20, 60, 28,
  35, 03, 43, 11, 51, 19, 59, 27,
  34, 02, 42, 10, 50, 18, 58, 26,
  33, 01, 41, 09, 49, 17, 57, 25]

-- Permuted choice 1 (PC-1)
-- 64 bit input
-- 56 bit output
permutedChoice1 :: Bits a => a -> a
permutedChoice1 = applyTable 64 [
  57, 49, 41, 33, 25, 17, 09, 01,
  58, 50, 42, 34, 26, 18, 10, 02,
  59, 51, 43, 35, 27, 19, 11, 03,
  60, 52, 44, 36, 63, 55, 47, 39,
  31, 23, 15, 07, 62, 54, 46, 38,
  30, 22, 14, 06, 61, 53, 45, 37,
  29, 21, 13, 05, 28, 20, 12, 04]

-- Permuted choice 2 (PC-2)
-- 56 bit input
-- 48 bit output
permutedChoice2 :: Bits a => a -> a
permutedChoice2 = applyTable 56 [
  14, 17, 11, 24, 01, 05,
  03, 28, 15, 06, 21, 10,
  23, 19, 12, 04, 26, 08,
  16, 07, 27, 20, 13, 02,
  41, 52, 31, 37, 47, 55,
  30, 40, 51, 45, 33, 48,
  44, 49, 39, 56, 34, 53,
  46, 42, 50, 36, 29, 32]