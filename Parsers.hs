module Parsers where

import Control.Applicative (Alternative, (<|>), empty, some, many)
import Data.Char (isDigit, isLetter, isSpace)



-- Parser combinators

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = P (\inp -> map (\(v, out) -> (f v, out)) (parse p inp))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> concat $
                         map (\(g, out) -> parse (fmap g px) out) (parse pg inp))

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> [(y, s) | (x, s') <- parse p inp, (y, s) <- parse (f x) s'])

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           rs -> rs)



-- discard any leading spaces
space :: Parser ()
space = do
  many $ sat isSpace
  return ()


-- eliminates spaces around the pure token we want
token :: Parser a -> Parser a
token p = do
  space
  x <- p
  space
  return x


sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

item :: Parser Char
item = P (\inp -> case inp of
    []     -> []
    (x:xs) -> [(x,xs)])



letter :: Parser Char
letter = sat isLetter

digit :: Parser Char
digit = sat isDigit


char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)



nat :: Parser Int
nat = do xs <- some digit
         return (read xs)