module Utils where

import Data.Maybe

first :: (a -> c) -> (a, b) -> (c, b)
first f ~(a, b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f ~(a, b) = (a, f b)

first3 :: (a -> c) -> (a, b, d) -> (c, b, d)
first3 f ~(a, b, c) = (f a, b, c)

second3 :: (b -> c) -> (a, b, d) -> (a, c, d)
second3 f ~(a, b, c) = (a, f b, c)

third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f ~(a, b, c) = (a, b, f c)

fst3 :: (a, b, c) -> a
fst3 ~(x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 ~(_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 ~(_, _, x) = x

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
  res <- cond
  if res then ifTrue else ifFalse

(!-!) :: Maybe a -> Maybe a -> Maybe a
l !-! r = if isJust l then l else r
