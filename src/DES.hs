module DES where

import Debug.Trace
import Data.Word
import Data.Bits 
import Data.Char
import Numeric (showIntAtBase)
import Test.HUnit

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
  in reverse [(x `shiftR` distance) .&. (mask outputSize) | distance <- distances]

-- same as with ungroup with the types
-- does not always work - when the end of the input is zeros, it will think its at the end
{- group :: Bits a => Int -> a -> [a]
group size x
  | popCount x == 0 = []
  | otherwise =
    let y = x .&. mask size
        xs = group size (x `shiftR` size)
    in xs ++ [y] -}

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
encrypt key nonce = counterMode key nonce . padPKCS7 8

decrypt :: Key -> Nonce -> [Word] -> [Word]
decrypt key nonce = unpadPKCS7 . counterMode key nonce


----------------------------------------------------------------
--                        PKCS7 Padding                       --
----------------------------------------------------------------

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

-- gets list of bytes, and returns lists of 64 bit word blocks
getBlocks :: [Word] -> [Word]
getBlocks bytes = map (ungroup 8) (chunks 8 bytes)

mergeCounterBlock :: Nonce -> Int -> Word
mergeCounterBlock nonce counter =
  (nonce `shiftL` 32) .|. ((fromIntegral counter) .&. 0xFFFFFFFF)

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
des key = finalPermutation . crypt . initialPermutation
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



  ------------------------------------------------------------------
--                            Tests                             --
------------------------------------------------------------------



testDES :: Test
testDES = TestList [
  let key =       0b0111010100101000011110000011100101110100100100111100101101110000
      plaintext = 0b0001000100100010001100110100010001010101011001100111011110001000
      target =    0b1011010100100001100111101110100000011010101001110100100110011101
  in TestCase $ assertEqual "des failed 1" target (des key plaintext),

  let key =       0b0111010100101000011110000011100101110100100100111100101101110000
      plaintext = 0b0000000000000000000000000000000000000000000000000000000000000000
      target =    0b0101011001110010000111100100000010100110100110010001010010101110
  in TestCase $ assertEqual "des failed 2" target (des key plaintext),

  let key =       0b0111010100101000011110000011100101110100100100111100101101110000
      plaintext = 0b0000000000000000000000000000000000000000000000000000000000000001
      target =    0b0101010001110010010000000100000100101100110110000011101000000110
  in TestCase $ assertEqual "des failed 3" target (des key plaintext),

  let key =       0b1101111101100001011010001110100000111010101101111001000100011110
      plaintext = 0b1000111001100000110111100000101010010111111001001110111001000001
      target =    0b0000000101000101001001001101100100111111010010000010011111011001
  in TestCase $ assertEqual "des failed 4" target (des key plaintext),

  let key =       0b1011110110001010000100011000110101101010001111101010000110000010 
      plaintext = 0b1000110011110110111110000011101001111100100111011000000110101111
      target =    0b1111110110000100101110111011000000101001100011000011011101000000
  in TestCase $ assertEqual "des failed 5" target (des key plaintext),

  let key =       0b0100101101000111100010010001100001101000011100101000100000100000
      plaintext = 0b0110111100011010110000111011111001010101110001001001110110101011
      target =    0b1100101110101111011001110010101110011010010010010110110100001001
  in TestCase $ assertEqual "des failed 6" target (des key plaintext),

  let key =       0b0100101101000111100010010001100001101000011100101000100000100000
      plaintext = 0b0000000000000000000000000000000000000000000000000000000000000000
      target =    0b1100010010110011011111000001110011110111000110100011100101011011
  in TestCase $ assertEqual "des failed 7" target (des key plaintext)
  ]

testPadding :: Test
testPadding = TestList [
  padPKCS7 8 (stringToBytes "YELLOWS") ~?= (stringToBytes "YELLOWS" ++ replicate 1 1),
  padPKCS7 8 (stringToBytes "YELLOWSU") ~?= (stringToBytes "YELLOWSU" ++ replicate 8 8),
  padPKCS7 16 (stringToBytes "YELLOWSUBMARINE") ~?= (stringToBytes "YELLOWSUBMARINE" ++ replicate 1 1),
  padPKCS7 16 (stringToBytes "YELLOWSUBMARINE!") ~?= (stringToBytes "YELLOWSUBMARINE!" ++ replicate 16 16),
  unpadPKCS7 (stringToBytes "YELLOWSUBMARINE!" ++ replicate 16 16) ~?= (stringToBytes "YELLOWSUBMARINE!")]

testCounterMode2 :: Test
testCounterMode2 = 
  let key = 0b0111010100101000011110000011100101110100100100111100101101110000
      nonce = 0b0000000000000000000000000000000000000000000000000000000000000000
      plaintext = [0b00010001,0b00100010,0b00110011,0b01000100,0b01010101,0b01100110,0b01110111,0b10001000]

      target = [0b01000111,0b01010000,0b00101101,0b00000100,0b11110011,0b11111111,0b01100011,0b00100110,
                0b01011100,0b01111010,0b01001000,0b01001001,0b00100100,0b11010000,0b00110010,0b00001110]
  in TestCase $ assertEqual "ctr failed" target (counterMode key nonce $ padPKCS7 8 plaintext)

tests :: Test
tests = TestList [
  testDES,
  testPadding,
  testFull,
  testCounterMode]

testFull :: Test
testFull = TestList [

    let message = 0b0000000000000000000000000000000000000000000000000000000000000000
        key = 0b0111010100101000011110000011100101110100100100111100101101110000
        target = 0b0101011001110010000111100100000010100110100110010001010010101110
    in TestCase $ assertEqual "des failed 2" target (des key message),

    let input = 0
        target = 0
    in TestCase $ assertEqual "ip" target (initialPermutation input),

    let key = 0b0111010100101000011110000011100101110100100100111100101101110000
        target = [
          0b001110001010110011101111010001100101011001001010,
          0b100010011011111011010100010010001001110100010010,
          0b010101000111111011101110010011010100010000111100,
          0b111100101111010101100000010010010101100011001000,
          0b110010001100111101100111100000001101000000111101,
          0b111000011111001100011111100000110001111010100100,
          0b001001011001011111100011100110000000101110110001,
          0b111100110101100011110011000100110100101000010101,
          0b000011001101101001111011101000000000101011000110,
          0b101001110111100101011110100101001010001010010111,
          0b001011100110111111000001001101110000011011000001,
          0b010110110111110100111001000110101010000101000011,
          0b110011011010010111011001001001101110010100000100,
          0b010101111100111010001111011010000010010111000010,
          0b011110111011100110000010111011001100000000001011,
          0b110100110011101000101101001000111000110101101000]
    in TestCase $ assertEqual "ks" target (keySchedule key),

    let right = 0
        target = 0
    in TestCase $ assertEqual "exp" target (expansionFunction right),

    let input = 0 :: Word
        target = 0b001110001010110011101111010001100101011001001010 :: Word
        subkey1 = 0b001110001010110011101111010001100101011001001010 :: Word
    in TestCase $ assertEqual "xor" target (xor subkey1 input),

    let input = 0b001110001010110011101111010001100101011001001010
        target = 0b10001011111110000101001000101111
    in TestCase $ assertEqual "sboxes" target (sboxes input),

    let input = 0b10001011111110000101001000101111
        target = 0b01101100101011110100110101100100
    in TestCase $ assertEqual "permute" target (permutation input),

    let right = 0
        left = 0
        subkey1 = 0b001110001010110011101111010001100101011001001010
        target = 0b0000000000000000000000000000000001101100101011110100110101100100
    in TestCase $ assertEqual "fiestial" (halves 64 target) (feistel subkey1 (left, right)),

    let input = 0b0000101101100111110101010010000010110000100100101010010010010111
        target = 0b0101011001110010000111100100000010100110100110010001010010101110
    in TestCase $ assertEqual "fp" target (finalPermutation input)

  ]


testCounterMode :: Test
testCounterMode = TestList [

    let string = "Current player: White\n\n8 - - - - - - - - \n7 - - - - - - - - \n6 - w - w - w - w \n5 - - - - - - - - \n4 - - - - - - - - \n3 b - b - b - b - \n2 - - - - - - - - \n1 - - - - - - - - \n  A B C D E F G H \n"
        key = 0b0110100101110111011100100111001101101110011001100110100001101100
        nonce = 0
        ciphertext = encrypt key nonce (stringToBytes string)
        ciphertextString = showBytes ciphertext
        target = "0110011001110101110000101010001110111111110110101010111110100111011011101011111010100101110101010100010011100101010111000111001110010101111111101010010100111110001011100000110100010110110110000010100100001101000000000011110011101110011101011000000100011110001101011001110111110000111100111010001111111011001011000111110000100001110000100111001001011100101101100001100010010001001000001010101000010010111101011111001110010000111111111101110001101001101111111000101001101011110110010000110011000011001000001001000110110111001100111010011010001010100111101111011011110011101011111000010111111110001110001111110000101111011001001000101011010100101011010000110101110101100101010110101100001000110100110000000001001101111110001000001011100011110001011101111011011110000001100010011100010010011011011010111110001000010101100100010010111111100011111101100110001011111101101101100111001011111100110111111011111011111000101010010000110101000111000001101111100101010001001010111101100011000000110011001100100100010010001101110010110111110110010100000011001001000110010101001001100001000000111111110000000100000111100011110111101100011001011011111010100001001001000100100010011000100001000100001101000010100010010110010011110011100000111100110110110100000000010000010001000110101011001000111001101100010110101000001010100001010000100011111011000110100000100011110001110111110100001110001010001000110010011100000110101010000100110100011011000000110011001101101110001001100110100000000011100010000010111111011100000111111001000111111111110100010011000000011110001000011000000100011010001101111011100011100000001101"
    in TestCase $ assertEqual "full" target ciphertextString,

    let bytes = [0b01101000, 0b01100101, 0b01101100, 0b01101100,
                 0b01101111, 0b00100000, 0b01110100, 0b01101000,
                 0b01100101, 0b01110011, 0b01100101, 0b00100000,
                 0b01100001, 0b01110010, 0b01100101, 0b00100000,
                 0b01110011, 0b01101111, 0b01101101, 0b01100101,
                 0b00100000, 0b01100010, 0b01111001, 0b01110100, 0b01100101, 0b01110011]
        target = [0b0110100001100101011011000110110001101111001000000111010001101000,
                  0b0110010101110011011001010010000001100001011100100110010100100000,
                  0b0111001101101111011011010110010100100000011000100111100101110100,
                  0b0110010101110011]
    in TestCase $ assertEqual "blocks" target (getBlocks bytes),


    let nonce = 0b00000000000000000000000000000000
        counter = 50
        target = 0b0000000000000000000000000000000000000000000000000000000000110010
    in TestCase $ assertEqual "mergecounterblock" target (mergeCounterBlock nonce counter)
  ]