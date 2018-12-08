module Data.List.Extended
    ( module Data.List
    , select
    , (!!?)
    ) where

import           Control.Monad (guard)
import           Data.List
import           Data.Maybe    (listToMaybe)

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

-- | Like '!!', but returns a 'Maybe'.
(!!?) :: [a] -> Int -> Maybe a
list !!? idx = guard (idx >= 0) >> listToMaybe (drop idx list)
