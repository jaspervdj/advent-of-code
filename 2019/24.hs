{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Main
    ( main
    ) where

import qualified AdventOfCode.Grid   as G
import qualified AdventOfCode.V2     as V2
import qualified AdventOfCode.V2.Box as Box
import           Control.Monad       (guard)
import qualified Data.List           as L
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, listToMaybe)
import qualified Data.Set            as Set
import qualified System.IO           as IO

type Grid = G.Grid Bool

bug :: Int -> Bool -> Bool
bug n True  = n == 1
bug n False = n == 1 || n == 2

stepGrid :: Grid -> Grid
stepGrid g = Map.mapWithKey
    (\p -> bug $ length $
        filter (fromMaybe False . (`Map.lookup` g)) $ G.neighbours p)
    g

biodiversity :: Grid -> Int
biodiversity g = case G.box g of
    Nothing -> 0
    Just (Box.Box (V2.V2 minX minY) (V2.V2 maxX maxY)) -> snd $ L.foldl'
        (\(n, acc) pos ->
            let b     = fromMaybe False $ Map.lookup pos g
                !acc' = if b then acc + n else acc in
            (n * 2, acc'))
        (1, 0)
        [V2.V2 x y | y <- [minY .. maxY], x <- [minX .. maxX]]

twice :: Ord a => [a] -> Maybe a
twice = go Set.empty
  where
    go _ []       = Nothing
    go v (x : xs) = if x `Set.member` v then Just x else go (Set.insert x v) xs

-- | Finds neighbours on the above, this, and the lower recursion level.
rneighbours :: (Int, Int) -> G.Pos -> ([G.Pos], [G.Pos], [G.Pos])
rneighbours (w, h) pos@(V2.V2 px py)
    | pos == center = ([], [], [])  -- Does not exist.
    | otherwise     = (above, this, below)
  where
    center@(V2.V2 cx cy) = V2.V2 (w `div` 2) (h `div` 2)

    this = do
        p@(V2.V2 x y) <- G.neighbours pos
        guard $ p /= center && x >= 0 && x < w && y >= 0 && y < h
        pure p

    above =
        [V2.V2 cx (cy - 1) | py <= 0] ++
        [V2.V2 (cx + 1) cy | px + 1 >= w] ++
        [V2.V2 cx (cy + 1) | py + 1 >= h] ++
        [V2.V2 (cx - 1) cy | px <= 0]

    below =
        [V2.V2 x 0       | px == cx, py + 1 == cy, x <- [0 .. w - 1]] ++
        [V2.V2 (w - 1) y | px - 1 == cx, py == cy, y <- [0 .. h - 1]] ++
        [V2.V2 x (h - 1) | px == cx, py - 1 == cy, x <- [0 .. w - 1]] ++
        [V2.V2 0 y       | px + 1 == cx, py == cy, y <- [0 .. h - 1]]

type Level = G.Grid Bool
data RGrid = RGrid [Level] Level [Level]

stepRGrid :: RGrid -> RGrid
stepRGrid (RGrid above lvl0 below) = RGrid
    (goUp (Just lvl0) above)
    (stepLevel dim (listToMaybe above, Just lvl0, listToMaybe below))
    (goDown (Just lvl0) below)
  where
    dim = (5, 5)

    goUp Nothing [] = []
    goUp mbBelow gs =
        stepLevel dim (listToMaybe $ drop 1 gs, listToMaybe gs, mbBelow) :
        goUp (listToMaybe gs) (drop 1 gs)

    goDown Nothing [] = []
    goDown mbAbove gs =
        stepLevel dim (mbAbove, listToMaybe gs, listToMaybe $ drop 1 gs) :
        goDown (listToMaybe gs) (drop 1 gs)

stepLevel
    :: (Int, Int)
    -> (Maybe Level, Maybe Level, Maybe Level)
    -> G.Grid Bool
stepLevel _      (Nothing, Nothing, Nothing) = Map.empty
stepLevel (w, h) (mbAbove, mbThis,  mbBelow) = Map.mapWithKey
    (\pos _ ->
        let b = fromMaybe False $ mbThis >>= Map.lookup pos
            (labove, lthis, lbelow) = rneighbours (w, h) pos
            bugs =
                countBugs mbAbove labove +
                countBugs mbThis lthis +
                countBugs mbBelow lbelow in
        bug bugs b)
    proto
  where
    proto = Map.fromList
        [(V2.V2 x y, ()) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

    countBugs Nothing  = const 0
    countBugs (Just g) = length .  filter (fromMaybe False . (`Map.lookup` g))

bugsInRGrid :: RGrid -> Int
bugsInRGrid (RGrid above lvl0 below) =
    sum (map count above) + count lvl0 + sum (map count below)
  where
    count = Map.foldl' (\acc x -> if x then acc + 1 else acc) 0

main :: IO ()
main = do
    grid <- G.readGrid (pure . (== '#')) IO.stdin
    dup  <- maybe (fail "no dup") pure . twice $ iterate stepGrid grid
    G.printGrid IO.stderr $ fmap (\c -> if c then '#' else '.') grid
    print $ biodiversity dup
    print $ bugsInRGrid $ iterate stepRGrid (RGrid [] grid []) !! 200
