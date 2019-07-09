module Combinators.Expr where

import           Control.Monad
import           Control.Applicative
import           Combinators

data Operator m a = InfixR (m (a -> a -> a))
                  | InfixL (m (a -> a -> a))
                  | InfixN (m (a -> a -> a))
                  | Prefix (m (a -> a))
                  | TenaryR (m (m (a -> a -> a -> a)))

type Batch m a = ( [m (a -> a -> a)]
                 , [m (a -> a -> a)]
                 , [m (a -> a -> a)]
                 , [m (a -> a)]
                 , [m (a -> a)]
                 , [m (m (a -> a -> a -> a))])

classify :: Operator m a -> Batch m a -> Batch m a
classify (InfixR op) (r, l, n, pre, post, tern) = (op:r, l, n, pre, post, tern)
classify (InfixL op) (r, l, n, pre, post, tern) = (r, op:l, n, pre, post, tern)
classify (InfixN op) (r, l, n, pre, post, tern) = (r, l, op:n, pre, post, tern)
classify (Prefix op) (r, l, n, pre, post, tern) = (r, l, n, op:pre, post, tern)
classify (TenaryR op) (r, l, n, pre, post, tern) =
  (r, l, n, pre, post, op:tern)

makeExpression :: (Monad m, Alternative m) => m a -> [[Operator m a]] -> m a
makeExpression = foldl addPrecedence

addPrecedence :: (Monad m, Alternative m) => m a -> [Operator m a] -> m a
addPrecedence term ops = term' >>= \x -> choice [las x, ras x, return x]
  where
    (r, l, n, pre, post, tern) = foldr classify ([], [], [], [], [], []) ops

    term' = do
      f <- optional $ choice pre
      x <- term
      return $ maybe x ($ x) f

    las = chainL (choice l) term'

    ras = chainR (choice r) term'

chainL :: (Monad m, Alternative m) => m (a -> a -> a) -> m a -> a -> m a
chainL op p x = do
  f <- op
  y <- p
  let r = f x y
  chainL op p r <|> return r

chainR :: (Monad m, Alternative m) => m (a -> a -> a) -> m a -> a -> m a
chainR op p x = do
  f <- op
  r <- p >>= \y -> (chainR op p y) <|> return y
  return $ f x r
