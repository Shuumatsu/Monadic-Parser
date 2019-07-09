{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Stream where

class (Ord (Token s), Ord (Tokens s)) => Stream s where
  type Token s :: *

  type Tokens s :: *

  take1 :: s -> Maybe (Token s, s)
  takeN :: Int -> s -> Maybe (Tokens s, s)

instance Stream String where
  type Token String = Char

  type Tokens String = String

  take1 [] = Nothing
  take1 (x:xs) = Just (x, xs)

  takeN n s
    | n <= 0 = Just ("", s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)