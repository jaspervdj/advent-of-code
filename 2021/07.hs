{-# LANGUAGE BangPatterns #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Foldable           (toList)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Map                as M

type Position = Int
type Fuel = Int
type Crabs = M.Map Position [Int]  -- Tracks steps taken for each crab.
type Consumption = Int -> Fuel

makeCrabs :: NonEmpty Position -> Crabs
makeCrabs = M.fromListWith (++) . flip zip (repeat [0]) . toList

-- Move the leftmost crabs one step to the right, or vice versa.
stepRight, stepLeft :: Consumption -> Crabs -> Maybe (Crabs, Fuel)
stepRight f crabs = do
    ((p, steps), crabs') <- M.minViewWithKey crabs
    pure (M.insertWith (++) (p + 1) (map succ steps) crabs', sum $ map f steps)
stepLeft f crabs = do
    ((p, steps), crabs') <- M.maxViewWithKey crabs
    pure (M.insertWith (++) (p - 1) (map succ steps) crabs', sum $ map f steps)

solve :: Consumption -> Crabs -> Either String Fuel
solve f = maybe (Left "no solution") Right . go 0
  where
    go !fuel crabs
         | M.size crabs == 1 = Just fuel
         | otherwise         = do
             (lc, lf) <- stepLeft f crabs
             (rc, rf) <- stepRight f crabs
             if lf < rf then go (fuel + lf) lc else go (fuel + rf) rc

main :: IO ()
main = pureMain $ \input -> do
    crabs <- makeCrabs <$> P.runParser (P.decimal `P.sepBy1` P.char ',') input
    pure (solve (const 1) crabs, solve succ crabs)
