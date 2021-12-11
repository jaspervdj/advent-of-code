{-# LANGUAGE BangPatterns #-}
module Main where

import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main (pureMain)
import           Data.Char         (digitToInt, isDigit)
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Traversable  (for)

step :: G.Grid Int -> (G.Grid Int, Int)
step = \grid0 -> go 0 grid0 (M.keys grid0)
  where
    go flashes grid []       = (bound <$> grid, flashes)
    go flashes grid (p : ps) = case M.lookup p grid of
        Nothing  -> go flashes grid ps
        Just val -> case compare val 9 of
            GT -> go flashes grid ps
            LT -> go flashes (M.insert p (val + 1) grid) ps
            EQ -> go (flashes + 1) (M.insert p (val + 1) grid) $
                ps ++ G.neighbours p ++ G.diagonal p

    bound x = if x > 9 then 0 else x

main :: IO ()
main = pureMain $ \input -> do
    grid <- for (G.fromString input) $ \c ->
        if isDigit c then Right (digitToInt c) else Left ("Bad char: " ++ [c])

    let grids = iterate (step . fst) (grid, 0)
        part1 = sum . map snd $ take (100 + 1) grids
        part2 = maybe (Left "no solution") Right $
            L.findIndex ((== M.size grid) . snd) grids

    pure (pure part1, part2)
