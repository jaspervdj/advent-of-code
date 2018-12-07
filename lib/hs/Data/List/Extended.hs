module Data.List.Extended
    ( module Data.List
    , select
    ) where

import           Data.List

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]
