{-# LANGUAGE BangPatterns #-}
module Main where

import qualified AdventOfCode.Loop    as Loop
import qualified AdventOfCode.Parsing as Parsing
import           AdventOfCode.V3
import qualified AdventOfCode.V3      as V3
import           Control.Monad        (guard)
import qualified Data.Foldable        as F
import qualified Data.List.Extended   as L
import qualified Data.Map             as Map
import           Debug.Trace
import qualified System.IO            as IO

data PosVel = PosVel !Int !Int deriving (Eq, Ord, Show)

type Moon = PosVel

gravity :: [Moon] -> Moon -> Int
gravity moons (PosVel m _) =
    L.sum [single m n | PosVel n _ <- moons]
  where
    single = \l r -> case compare l r of EQ -> 0; LT -> 1; GT -> -1

readMoons :: IO.Handle -> IO [V3 Moon]
readMoons h =
    IO.hGetContents h >>= mapM readMoon . lines
  where
    readMoon line = case Parsing.ints line of
        [x, y, z] -> pure $ V3 (PosVel x 0) (PosVel y 0) (PosVel z 0)
        _         -> fail $ "Could not parse line: " ++ line

step :: [Moon] -> Moon -> Moon
step moons moon@(PosVel p v) =
    let g = gravity moons moon
        v' = v + g in
    PosVel (p + v') v'

stepAll :: [V3 Moon] -> [V3 Moon]
stepAll moons =
    map (\v3 -> V3.mapWithIndex (\idx -> step (map idx moons)) v3) moons

energy :: V3 Moon -> Int
energy (V3 (PosVel p1 v1) (PosVel p2 v2) (PosVel p3 v3)) =
    (abs p1 + abs p2 + abs p3) * (abs v1 + abs v2 + abs v3)

{-
-- Quadratic equation form for a moon.
data Curve a = Curve a a a deriving (Show)

evalCurve :: Num a => a -> Curve a -> a
evalCurve x (Curve a b c) = a * x * x + b * x + c

zeroCurve :: Curve Double -> [Double]
zeroCurve (Curve a b c)
    | a == 0.0  = if b == 0.0 then [] else [-c / b]
    | d < 0     = []
    | d == 0.0  = [(-b + sqrt d) / (2 * a)]
    | otherwise = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
  where
    d = b * b - 4 * a * c

solveCurves :: Curve Double -> Curve Double -> [Double]
solveCurves (Curve a0 b0 c0) (Curve a1 b1 c1) =
    zeroCurve $ Curve (a0 - a1) (b0 - b1) (c0 - c1)

moonToCurves :: V3 Int -> Moon -> V3 (Curve Double)
moonToCurves acc (Moon pos vel) =
    V3.zipWith3 mkCurve pos vel acc
  where
    mkCurve p v a = Curve
        (fromIntegral a / 2)
        (fromIntegral a / 2 + fromIntegral v)
        (fromIntegral p)

-- Skip N steps with constant acceleration.
skip :: Int -> V3 Int -> Moon -> Moon
skip n acc (Moon pos0 vel0) =
    let pos = pos0 .+. vel0 .* n .+. acc .* ((n * (n + 1)) `div` 2)
        vel = vel0 .+.  acc .* n in
    Moon pos vel

-- Pick two distinct items.
select2 :: [a] -> [(a, a)]
select2 ls = do
    (x, ls') <- L.select ls
    (y, _)   <- L.select ls'
    pure (x, y)

multistep :: [Moon] -> ([Moon], Int)
multistep moons =
    (L.zipWith (skip stable) accs moons, stable)
  where
    accs   = gravity moons
    curves = L.zipWith moonToCurves accs moons
    stable = minimum $ do
        (c1, c2) <- select2 curves
        solution <- concatMap F.toList $ V3.zipWith solveCurves c1 c2
        guard $ solution >= 0.0
        pure $ floor solution

simulate :: [Moon] -> Int
simulate moons0 = go Map.empty 0 moons0
  where
    go seen !i moons = case Map.lookup moons seen of
        Just j  -> i - j
        Nothing -> case multistep moons of
            (_, 0)      -> go seen (i + 1) (step moons)
            (moons', d) ->
                go (Map.insert moons i seen) (i + d) moons'

test_moons =
    [ Moon (V3 1 2 1)             (V3 8 6 10)
    , Moon (V3 10000 10000 10000) (V3 0 0 0)
    ]
-}

main :: IO ()
main = do
    moons0 <- readMoons IO.stdin
    let moons1000 = iterate stepAll moons0 !! 1000
    print . L.sum $ map energy moons1000
    -- print $ simulate moons0
