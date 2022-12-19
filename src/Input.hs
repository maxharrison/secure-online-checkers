module Input where

import System.IO (hFlush, stdout)
import Types (Coordinate, Move)
import Parsers (Parser, parse, token, digit, letter)
import Control.Applicative ((<|>))
import Data.Char (isDigit, isLetter, digitToInt, toUpper, ord)


charToInt :: Char -> Int
charToInt c = (ord $ toUpper c) - 65


digToInt :: Char -> Int
digToInt c = (digitToInt c) - 1


digitLetter :: Parser (Int, Int)
digitLetter = do
  d <- (token digit)
  l <- (token letter)
  return (charToInt l, digToInt d)


letterDigit :: Parser (Int, Int)
letterDigit = do
  l <- (token letter)
  d <- (token digit)
  return (charToInt l, digToInt d)


coordinate :: String -> Maybe Coordinate
coordinate s = let result = parse (letterDigit <|> digitLetter) s
               in case result of
                    [] -> Nothing
                    (((l, d),_):_) -> Just (l,  d)


prompt :: String -> IO String
prompt s = do
   putStr $ s ++ ": "
   hFlush stdout
   getLine


getCoordinates :: IO (Move)
getCoordinates = do
    input1 <- prompt "\nPlease enter the coordinates of the piece you would like to move"
    case coordinate input1 of
         Nothing -> do putStrLn "\nInvalid input"
                       getCoordinates
         Just c1 -> do input2 <- prompt "Please enter the destination coordinates"
                       case coordinate input2 of
                            Nothing -> do putStrLn "\nInvalid input"
                                          getCoordinates
                            Just c2 -> return (c1, c2)