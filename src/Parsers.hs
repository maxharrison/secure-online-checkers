module Parsers where
{-
Inspiration from:
    Programming in Haskell, Graham Hutton, Cambridge University Press, 2016.
    COMP3012 Compilers module taught by Venanzio Capretta, University of Nottingham, 2022.
-}

-- Haskell modules
import Control.Applicative (Alternative, (<|>), empty, some, many)
import Data.Char (isDigit, isLetter, isSpace)


newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g px = P (\inp ->
        case parse px inp of
            []         -> []
            [(x, out)] -> [(g x, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])
    
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp ->
        case parse pg inp of
            []         -> []
            [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = P (\inp ->
        case parse px inp of
            []         -> []
            [(x, out)] -> parse (f x) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])
    
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp ->
        case parse p inp of
            [] -> parse q inp
            rs -> rs)

item :: Parser Char
item = P (\inp -> case inp of
    []     -> []
    (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then return x
        else empty

space :: Parser ()
space = do
    many $ sat isSpace
    return ()

token :: Parser a -> Parser a
token p = do
    space
    x <- p
    space
    return x

letter :: Parser Char
letter = sat isLetter

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

stringToken :: String -> Parser String
stringToken []     = return []
stringToken (x:xs) = token $ do
    char x
    stringToken xs
    return (x:xs)

someSeperator :: Parser a -> String -> Parser [a]
someSeperator p s = do
    x <- p
    stringToken s
    xs <- someSeperator p s
    return (x:xs)
    <|> do x <- p
           return [x]