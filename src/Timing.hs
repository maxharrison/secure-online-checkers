module Timing where

import ECDH
import DES
import System.CPUTime
import System.Random
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, CurveName(..))
import System.CPUTime
import Crypto.PubKey.ECC.Prim (pointMul)


main :: IO ()
main = do
  desTiming
  --keyPairTimings
  --sharedTimings


sharedTimings :: IO ()
sharedTimings = do

  let runs = 100

  times1 <- sequence $ replicate runs $ do
    curve <- return $ getCurveByName SEC_p256k1
    (pub, _) <- generate curve
    (_, priv) <- generate curve
    start1 <- getCPUTime
    let sharedSecret = pointMul curve (private_d priv) (public_q pub)
    print $ sharedSecret
    end1 <- getCPUTime
    return $ fromIntegral (end1 - start1) / (10^12)

  times2 <- sequence $ replicate runs $ do
    (_, myPublicKey) <- generateKeyPair_Secp256k1
    (myPrivateKey, _) <- generateKeyPair_Secp256k1
    start2 <- getCPUTime
    let secret = sharedSecret_Secp256k1 myPrivateKey myPublicKey
    print myPublicKey
    end2 <- getCPUTime
    return $ fromIntegral (end2 - start2) / (10^12)

  putStrLn ("1 time taken: " ++ show (sum times1 / fromIntegral runs) ++ " seconds")
  putStrLn ("2 time taken: " ++ show (sum times2 / fromIntegral runs) ++ " seconds")




keyPairTimings :: IO ()
keyPairTimings = do

  let runs = 100

  times1 <- sequence $ replicate runs $ do
    start1 <- getCPUTime
    let curve = getCurveByName SEC_p256k1
    (thirdPartyPrivateKey, thirdPartyPublicKey) <- generate curve
    print thirdPartyPrivateKey
    print thirdPartyPublicKey
    end1 <- getCPUTime
    return $ fromIntegral (end1 - start1) / (10^12)

  times2 <- sequence $ replicate runs $ do
    start2 <- getCPUTime
    (myPrivateKey, myPublicKey) <- generateKeyPair_Secp256k1
    print myPrivateKey
    print myPublicKey
    end2 <- getCPUTime
    return $ fromIntegral (end2 - start2) / (10^12)

  putStrLn ("1 time taken: " ++ show (sum times1 / fromIntegral runs) ++ " seconds")
  putStrLn ("2 time taken: " ++ show (sum times2 / fromIntegral runs) ++ " seconds")

  

desTiming :: IO ()
desTiming = do
  -- Define the key and plaintext
  let key = 0b0111010100101000011110000011100101110100100100111100101101110000
  let plaintext = 0b0001000100100010001100110100010001010101011001100111011110001000

  -- Define the number of runs
  let runs = 5000

  -- Collect the CPU time for each run
  times <- sequence $ replicate runs $ do
    gen <- newStdGen
    let (num, _) = randomR (1, 1000) gen :: (Word, StdGen)
    start <- getCPUTime
    let result = des key (plaintext + num)
    print result
    end <- getCPUTime
    return $ fromIntegral (end - start) / (10^12)

  -- Calculate the average time taken
  let avgTime = sum times / fromIntegral runs

  -- Print the average time taken
  putStrLn ("Average time taken: " ++ show avgTime ++ " seconds")



