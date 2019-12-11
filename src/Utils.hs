module Utils where

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

