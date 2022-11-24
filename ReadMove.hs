module ReadMove where

import Parsers (Parser, parse, token, digit, letter)
import Control.Applicative ((<|>))
import Data.Char (isDigit, isLetter, digitToInt, toUpper, ord)
import Types 

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