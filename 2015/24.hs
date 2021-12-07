{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad   (guard)
import           Data.List       (sortOn)
import           Data.List.Extra (select)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified System.IO       as IO

readWeights :: IO.Handle -> IO [Int]
readWeights h = map read . lines <$> IO.hGetContents h

findSums :: Int -> [Int] -> [Set Int]
findSums n = go Set.empty 0
  where
    go set k ls = case compare k n of
        GT -> []
        EQ -> [set]
        LT -> case ls of
            []     -> []
            x : xs -> go (Set.insert x set) (k + x) xs ++ go set k xs

distribute :: Int -> [Int] -> [Int]
distribute n weights = do
    (group1, remainder) <- select $
        sortOn (\x -> (Set.size x, product x)) $
        findSums (sum weights `div` n) weights
    guard $ validate 1 group1 remainder
    pure $ product group1
  where
    validate :: Int -> Set Int -> [Set Int] -> Bool
    validate i acc remainder
        | i >= n    = True
        | otherwise = or $ do
            (g, remainder') <- select remainder
            return $ validate (i + 1) (acc <> g) remainder'

main :: IO ()
main = do
    weights <- readWeights IO.stdin
    print . head $ distribute 3 weights
    print . head $ distribute 4 weights
