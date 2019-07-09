module Parsec.Char.Lexer where

import           Data.Char (isDigit, isAlpha)
import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Parsec

char :: Char -> Parsec String Char
char c = satisfy (== c)

spaceConsumer :: Parsec String ()
spaceConsumer = (some $ char ' ') $> ()

lexem :: Parsec String a -> Parsec String a
lexem p = p <* (many spaceConsumer)

string :: String -> Parsec String String
string [] = pure []
string (c:cs) = liftA2 (:) (char c) (string cs)

decimal :: Parsec String Double
decimal = do
  x <- some $ satisfy isDigit
  dot <- optional $ satisfy (== '.')
  case dot of
    Nothing -> return . read $ x
    Just _  -> do
      y <- some $ satisfy isDigit
      return . read $ x ++ "." ++ y