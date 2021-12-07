module Main where

import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import qualified System.IO               as IO

data MinCount a = MinCount !Int a

mkMinCount :: a -> MinCount a
mkMinCount = MinCount 1

getMinCount :: MinCount a -> Int
getMinCount (MinCount c _) = c

instance Ord a => Semigroup (MinCount a) where
    MinCount 0  _ <> MinCount yc y = MinCount yc y
    MinCount xc x <> MinCount 0  _ = MinCount xc x
    MinCount xc x <> MinCount yc y = case compare x y of
        LT -> MinCount xc        x
        EQ -> MinCount (xc + yc) x
        GT -> MinCount yc        y

instance Ord a => Monoid (MinCount a) where
    mempty  = MinCount 0 undefined
    mappend = (<>)

store :: Int -> [Int] -> [[Int]]
store 0 _        = [[]]
store _ []       = []
store n (x : xs)
    | n < x      = store n xs
    | otherwise  =
        map (x :) (store (n - x) xs) ++
        store n xs

main :: IO ()
main = do
    containers <- NP.hRunParser IO.stdin (many $ NP.decimal <* NP.spaces)
    print $ length $ store 150 containers
    print $ getMinCount $ foldMap (mkMinCount . length) $ store 150 containers
