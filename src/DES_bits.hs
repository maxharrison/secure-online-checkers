module DES_bits where

import Debug.Trace
import Data.Word
import Data.Bits 
import Data.Char
import Numeric (showIntAtBase)





------------------------------------------------------------------
--                            Types                             --
------------------------------------------------------------------

type Key = Word
type Nonce = Word
type Counter = Word

------------------------------------------------------------------
--                       Helper Functions                       --
------------------------------------------------------------------

halves :: Bits a => Int -> a -> (a, a)
halves size bits = (bits `shiftR` half_size, bits .&. mask half_size)
  where half_size = size `div` 2

unhalves :: Bits a => Int -> (a, a) -> a
unhalves size (left, right) = (right .|. (left `shiftL` half_size)) .&. mask size
    where half_size = size `div` 2

-- same as with ungroup with the types
groupNew :: Int -> Int -> Word -> [Word]
groupNew outputSize inputSize x =
  let n = inputSize `div` outputSize
      distances = map ((*) outputSize) [0..(n-1)]
  in reverse $ [(x `shiftR` distance) .&. (mask outputSize) | distance <- distances]

-- same as with ungroup with the types
-- does not always work - when the end of the input is zeros, it will think its at the end
group :: Bits a => Int -> a -> [a]
group size x
  | popCount x == 0 = []
  | otherwise =
    let y = x .&. mask size
        xs = group size (x `shiftR` size)
    in xs ++ [y]

-- did have this type ungroup :: Bits a => Int -> [a] -> a, but it meant if
  -- you were ungrouping 2 Words, they were just returning a Word,
  -- so half the bits would be ignored
ungroup :: Int -> [Word] -> Word
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

getBit :: Bits a => a -> Int -> a
getBit x n = if testBit x n then one else zero

bytesToString :: [Word] -> String
bytesToString = map (chr . fromIntegral)

stringToBytes :: String -> [Word]
stringToBytes = map (fromIntegral . ord)

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

showBinary b = showIntAtBase 2 intToDigit b ""

showByte :: Word -> String
showByte b =
  let string = showIntAtBase 2 intToDigit b ""
  in replicate (8-(length string)) '0' ++ string

showBytes :: [Word] -> String
showBytes = concatMap showByte

------------------------------------------------------------------
--                 Encrypt/Decrypt Functions                    --
------------------------------------------------------------------



encrypt :: Key -> Nonce -> [Word] -> [Word]
--encrypt key nonce = ctrMode des key nonce . padPKCS7 8
encrypt key nonce bytes = (counterMode key nonce . padPKCS7 8) bytes

decrypt :: Key -> Nonce -> [Word] -> [Word]
--decrypt key nonce = unpadPKCS7 . ctrMode des key nonce
decrypt key nonce bytes = (unpadPKCS7 . counterMode key nonce) bytes



----------------------------------------------------------------
--                        PKCS7 Padding                       --
----------------------------------------------------------------


-- int is the number of bytes in a block
padPKCS7 :: Int -> [Word] -> [Word]
padPKCS7 block_bytes_len bytes = bytes ++ padding
  where bytes_len = length bytes
        padding_len = block_bytes_len - (bytes_len `mod` block_bytes_len)
        padding = replicate padding_len (fromIntegral padding_len)


unpadPKCS7 :: [Word] -> [Word]
unpadPKCS7 bytes = take len bytes
  where bytes_len = length bytes
        padding_len = fromIntegral $ last bytes
        len = bytes_len - padding_len



------------------------------------------------------------------
--                      CTR - Counter Mode                      --
------------------------------------------------------------------


{- ctrMode :: (Key -> Binary -> Binary) -> Key -> Nonce -> Binary -> Binary
ctrMode cipher key nonce binary =
  concat [ctrBlock cipher key nonce counter block | (counter, block) <- zip counters blocks]
    where counters = [(pad 32 . intToBinary) n | n <- [1..]]
          blocks = group 64 binary
          len = length blocks

ctrBlock :: (Key -> Binary -> Binary) -> Key -> Nonce -> Binary -> Binary -> Binary
ctrBlock cipher key nonce counter binary = binary `xor` (cipher key (nonce ++ counter))
 -}

--ctrMode :: (Key -> Word -> Word) -> Key -> Nonce -> [Word] -> [Word]
--ctrMode cipher key nonce bytes =
--  concat [ctrBlock cipher key nonce counter block | (counter, block) <- zip counters blocks]
--    where counters = [(fromIntegral n) | n <- [0..len]]
--          blocks = chunks 8 bytes
--          len = length blocks

--ctrBlock :: (Key -> Word -> Word) -> Key -> Nonce -> Counter -> [Word] -> [Word]
--ctrBlock cipher key nonce counter bytes =
--  group 8 $ (block_key `xor` block)
--    where counter_block = ungroup 32 [nonce, counter]
--          block_key = cipher key counter_block
--          block = ungroup 64 (bytes)

--ciphertextTest = des keyTest plaintextTest
{- counterMode :: Key -> Nonce -> [Word] -> [Word]
counterMode key nonce bytes = 
  let blocks = map (ungroup 8) (chunks 8 bytes)
      block_len = length blocks
      counter_blocks = [ungroup 32 [nonce, fromIntegral counter] | counter <- [0..block_len]]
      key_stream_blocks = map (des key) counter_blocks
  in concatMap (\(b,k) -> group 8 (b `xor` k)) $ zip blocks key_stream_blocks
    --trace (" -|S " ++ show (length $ bytes) ++ " E|- ") [block `xor` key_stream | (block, key_stream) <- zip blocks key_stream_blocks]
 -}

-- gets list of bytes, and returns lists of 64 bit word blocks
getBlocks :: [Word] -> [Word]
getBlocks bytes = map (ungroup 8) (chunks 8 bytes)

mergeCounterBlock :: Nonce -> Int -> Word
mergeCounterBlock nonce counter =
  (nonce `shiftL` 32) .|. (fromIntegral counter)

word64ToBytes :: Word -> [Word]
word64ToBytes w = [w `shiftR` (8 * i) .&. 0xFF | i <- [7,6..0]]

words64ToBytes :: [Word] -> [Word]
words64ToBytes ws = concatMap word64ToBytes ws

counterMode :: Key -> Nonce -> [Word] -> [Word]
counterMode key nonce bytes =
  let blocks = getBlocks bytes
      counter_blocks = [mergeCounterBlock nonce counter | counter <- [1..]]
      key_stream_blocks = map (des key) counter_blocks
      encrypted_blocks = zipWith xor blocks key_stream_blocks
      encrypted_bytes = words64ToBytes encrypted_blocks
  in encrypted_bytes




------------------------------------------------------------------
--                        DES Functions                        --
------------------------------------------------------------------


des :: Key -> Word -> Word
des key bits = (finalPermutation . crypt . initialPermutation) bits
  where keys = keySchedule key
        crypt = unhalves 64 . applyKeys keys . halves 64

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
sboxes = ungroup 4 . zipWith ($) boxes . groupNew 6 48
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
  where subKey' = (rotateHalves 56 shift subKey)

rotateHalves :: Bits a => Int -> Int -> a -> a
rotateHalves size n word =
  let (left, right) = halves size word
      left' = cShiftL halfSize n left
      right' = cShiftL halfSize n right
      halfSize = size `div` 2
  in unhalves size (left', right')


----------------------------------------------------------------
--                           S-Boxes                          --
----------------------------------------------------------------



applySbox :: [[Word]] -> Word -> Word
applySbox sbox input = (sbox !! row) !! col
  where
    row = fromIntegral $ (input .&. 0x20) `shiftR` 4 .|. (input .&. 0x1)
    col = fromIntegral $ (input .&. 0x1E) `shiftR` 1


--applySbox1 :: [[Word]] -> Word -> Word
--applySbox1 sbox bits = sbox !! row !! column
--  where row = fromIntegral $ getManyBits bits [5,0]
--        column = fromIntegral $ getManyBits bits [4,3,2,1]

getManyBits :: Word -> [Int] -> Word
getManyBits _ [] = zero
getManyBits bits (x:xs) =
  bit `shiftL` n .|. getManyBits bits xs
    where bit = getBit bits x
          n = length xs

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


applyTable :: Int -> [Int] -> Word -> Word
applyTable _ [] _ = zero
applyTable input_len (pos:xs) bits =
  let input_pos = input_len - pos
      input_bit = getBit bits input_pos
      output_pos = length xs
      output_bit = input_bit `shiftL` output_pos
  in output_bit .|. applyTable input_len xs bits




-- Permutation (P)
-- 32 bit input
-- 32 bit output
permutation :: Word -> Word
permutation = applyTable 32 [
  16, 07, 20, 21, 29, 12, 28, 17,
  01, 15, 23, 26, 05, 18, 31, 10,
  02, 08, 24, 14, 32, 27, 03, 09,
  19, 13, 30, 06, 22, 11, 04, 25]

-- Expansion function (E)
-- 32 bit input
-- 48 bit output
expansionFunction :: Word -> Word
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
initialPermutation :: Word -> Word
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
finalPermutation :: Word -> Word
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
permutedChoice1 :: Word -> Word
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
permutedChoice2 :: Word -> Word
permutedChoice2 = applyTable 56 [
  14, 17, 11, 24, 01, 05,
  03, 28, 15, 06, 21, 10,
  23, 19, 12, 04, 26, 08,
  16, 07, 27, 20, 13, 02,
  41, 52, 31, 37, 47, 55,
  30, 40, 51, 45, 33, 48,
  44, 49, 39, 56, 34, 53,
  46, 42, 50, 36, 29, 32]