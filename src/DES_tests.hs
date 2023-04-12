module DES_tests where

import Debug.Trace
import Data.Word
import Data.Bits 
import Data.Char
import Test.HUnit
import Numeric (showIntAtBase)
import DES_bits


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



main :: IO ()
main = do
  runTestTT tests
  putStrLn "Done"













