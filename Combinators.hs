module Combinators where

import           Control.Monad
import           Control.Applicative

choice :: Alternative m => [m a] -> m a
choice ps = foldl (<|>) empty ps

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = liftA2 (:) p (many $ sep *> p)

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []
