module Input where

-- Haskell modules
import System.IO (hFlush, stdout)
import Control.Applicative ((<|>))
import Data.Char (isDigit, isLetter, digitToInt, toUpper, ord)

-- My modules
import GameState
import Parsers


charToInt :: Char -> Int
charToInt c = (ord $ toUpper c) - 65

digToInt :: Char -> Int
digToInt c = (digitToInt c) - 1

digitLetterParser :: Parser (Int, Int)
digitLetterParser = do
    d <- (token digit)
    l <- (token letter)
    return (charToInt l, digToInt d)

letterDigitParser :: Parser (Int, Int)
letterDigitParser = do
    l <- (token letter)
    d <- (token digit)
    return (charToInt l, digToInt d)

positionParser :: Parser Position
positionParser = do
    (l, d) <- (digitLetterParser <|> letterDigitParser)
    return (l, d)

routeParser :: Parser [Position]
routeParser = do
    positions <- someSeperator positionParser ","
    return positions

promptUser :: String -> IO String
promptUser s = do
   putStrLn $ s ++ ":"
   hFlush stdout
   getLine

getRoute :: IO Route
getRoute = do
    input <- promptUser "\nPlease enter the move you would like to make (a list of coordinates separated by commas)"
    case parse routeParser input of
         [] -> do putStrLn "\nInvalid input"
                  getRoute
         ((route,_):_) -> do
            if length route < 2
                then do putStrLn "\nYou have to enter more than one coordinate"
                        getRoute
                else return route

