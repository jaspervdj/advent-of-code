module Data.List.Extra
    ( module L
    , select
    , selectN
    , powerset
    , (!!?)
    , lexicographicSuccessor
    , stripSuffix
    , safeLast
    ) where

import           Control.Monad  (guard)
import           Data.Bifunctor (first, second)
import qualified Data.List      as L
import           Data.Maybe     (listToMaybe)

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

selectN :: Int -> [a] -> [([a], [a])]
selectN n items = selectNL n (length items) items

selectNL :: Int -> Int -> [a] -> [([a], [a])]
selectNL n l items
    | n == 0    = [([], items)]
    | n == l    = [(items, [])]
    | n >  l    = []
    | otherwise = case items of
        [] -> []   -- Impossible
        (x : xs) ->
            map (first (x :)) (selectNL (n - 1) (l - 1) xs) ++
            map (second (x :)) (selectNL n (l - 1) xs)

powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

-- | Like '!!', but returns a 'Maybe'.
(!!?) :: [a] -> Int -> Maybe a
list !!? idx = guard (idx >= 0) >> listToMaybe (drop idx list)

lexicographicSuccessor :: (Bounded a, Enum a, Eq a) => [a] -> [a]
lexicographicSuccessor =
    \xs -> let (ys, carry) = go xs in if carry then minBound : ys else ys
  where
    go [] = ([], True)
    go (x : xs) =
        let (ys, carry) = go xs
            y           = if x == maxBound then minBound else succ x in
        if carry then (y : ys, x == maxBound) else (x : ys, False)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . L.stripPrefix (reverse suffix) . reverse

safeLast :: [a] -> Maybe a
safeLast ls = if null ls then Nothing else Just (last ls)
