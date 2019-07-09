module Parsec where

import           Control.Monad
import           Control.Applicative
import           Control.Monad.State.Strict
import           Stream

type Parsec s a = StateT s [] a

runParsec :: (Stream s) => Parsec s a -> s -> [(a, s)]
runParsec = runStateT

anySingle :: (Stream s) => Parsec s (Token s)
anySingle = do
  stream <- get
  case take1 stream of
    Nothing      -> empty
    Just (x, xs) -> do
      put xs
      return x

satisfy :: (Stream s) => (Token s -> Bool) -> Parsec s (Token s)
satisfy p = do
  c <- anySingle
  if p c
    then return c
    else empty
