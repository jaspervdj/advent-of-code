{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Grid  as G
import           AdventOfCode.V2
import           Control.Monad      (guard)
import qualified Data.List.Extended as L
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe, isNothing)
import qualified System.IO          as IO

-- | What other points are perfectly on this line?  This is pretty slow but that
-- doesn't matter too much.  The returned points are ordered in the direction of
-- 'p' to 'q'.
lineOfSight
    :: V2 Int -> V2 Int -> [V2 Int]
lineOfSight p q = do
    step <- [1 .. hi - 1]
    let (c, remainder) = (step * lo) `divMod` hi
    guard $ remainder == 0
    pure $ p .+. mkV2 c step
  where
    d               = q .-. p
    V2 adx ady      = fmap abs d
    V2 sx  sy       = fmap signum d
    (lo, hi, mkV2)
        | adx < ady = (adx, ady, \l r -> V2 (sx * l) (sy * r))
        | otherwise = (ady, adx, \l r -> V2 (sx * r) (sy * l))

-- | We only care about the position, asteroids don't have a value.
type Asteroid = ()

-- | When we look from one asteroid to the other, is the view blocked by a third
-- asteroid?  Note that this always returns the blocking asteroid closest to 'p'
-- if there are more than one.
blocked :: G.Grid Asteroid -> V2 Int -> V2 Int -> Maybe (V2 Int)
blocked grid p = L.find (`Map.member` grid) . lineOfSight p

-- | Number of asteroids visible from every position.
visible :: G.Grid Asteroid -> G.Grid Int
visible grid = Map.mapWithKey
    (\p _ -> length
        [() | (q, _) <- Map.toList grid, p /= q, isNothing (blocked grid p q)])
    grid

-- | Bucket all asteroids (except for the one you are standing on) by the first
-- asteroid you see in that direction.
asteroidsByBlocker
    :: G.Grid Asteroid -> V2 Int -> G.Grid [V2 Int]
asteroidsByBlocker grid viewpoint = Map.fromListWith (++)
    [ (fromMaybe p (blocked grid viewpoint p), [p])
    | (p, ()) <- Map.toList grid
    , p /= viewpoint
    ]

-- | Take the first item from the first list, then the first item from the
-- second list, and so on until all lists are empty.
popping :: [[a]] -> [a]
popping = go []
  where
    go acc []                 = if null acc then [] else go [] (reverse acc)
    go acc ((x : xs) : lists) = x : go (xs : acc) lists
    go acc ([] : lists)       = go acc lists

-- | Compute a key for sorting clockwise.
clockwise :: V2 Int -> Float
clockwise (V2 x y) =
    let r = atan2 (fromIntegral y) (fromIntegral x) in
    if r < -pi / 2 then 5 * pi / 2 + r else pi / 2 + r

-- | In what order do we laser asteroids?
laser :: G.Grid Asteroid -> V2 Int -> [V2 Int]
laser grid viewpoint =
    popping .
    map (L.sortOn (G.manhattan viewpoint) . snd) .
    L.sortOn (clockwise . (.-. viewpoint) . fst) .
    Map.toList $ asteroidsByBlocker grid viewpoint

-- | Parse a field of asteroids.
parseAsteriods :: IO.Handle -> IO (G.Grid Asteroid)
parseAsteriods =
    fmap (Map.mapMaybe (\a -> if a then Just () else Nothing)) .
    G.readGrid (\case
        '#' -> pure True
        '.' -> pure False
        c   -> fail $ "Can't parse: " ++ show c)

main :: IO ()
main = do
    asteroids <- parseAsteriods IO.stdin
    let (pos, count) = L.maximumOn snd . Map.toList $ visible asteroids
        V2 x y       = laser asteroids pos !! (200 - 1)
    print count
    print $ x * 100 + y
