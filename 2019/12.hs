{-# LANGUAGE BangPatterns #-}
module Main where

import qualified AdventOfCode.Parsing as Parsing
import           AdventOfCode.V3
import qualified AdventOfCode.V3      as V3
import qualified Data.List.Extended   as L
import qualified System.IO            as IO

-- The key insight is that while the different moons influence each other's
-- movements, the different components (x, y, z) of the moons are completely
-- independent.  This means we can run most of our code with a single component
-- ('PosVel') and then combine them.
--
-- Another trick that I implement before realizing this is that you can describe
-- the x, y, z positions as quadratic equations.  If you then solve all of the
-- different equations between the different moons, you can figure out the next
-- point in time where the moons will "cross" each other.  That is the point
-- where acceleration may change for some moons.  This can be used to then used
-- a closed form formula to skip until that point, and then start again using
-- quadratic equations.  However, using only this trick my code was still too
-- slow, and when I did the component split it turned out to be fast enough.
data PosVel = PosVel !Int !Int deriving (Eq, Ord, Show)
type Moon1  = PosVel
type Moon3  = V3 PosVel

gravity1 :: [Moon1] -> Moon1 -> Int
gravity1 moons (PosVel m _) = L.sum
    [case compare m n of EQ -> 0; LT -> 1; GT -> -1 | PosVel n _ <- moons]

step1 :: [Moon1] -> Moon1 -> Moon1
step1 moons moon@(PosVel p v) =
    let g = gravity1 moons moon; v' = v + g in PosVel (p + v') v'

step3 :: [Moon3] -> [Moon3]
step3 moons = V3.mapWithIndex (\idx -> step1 (idx <$> moons)) <$> moons

energy3 :: Moon3 -> Int
energy3 (V3 (PosVel p1 v1) (PosVel p2 v2) (PosVel p3 v3)) =
    (abs p1 + abs p2 + abs p3) * (abs v1 + abs v2 + abs v3)

readMoons :: IO.Handle -> IO [Moon3]
readMoons h =
    IO.hGetContents h >>= mapM readMoon . lines
  where
    readMoon line = case Parsing.ints line of
        [x, y, z] -> pure $ V3 (PosVel x 0) (PosVel y 0) (PosVel z 0)
        _         -> fail $ "Could not parse line: " ++ line

fixp :: Eq a => (a -> a) -> a -> Int
fixp f z = let go !i x = if z == x then i else go (i + 1) (f x) in go 1 (f z)

main :: IO ()
main = do
    moons0 <- readMoons IO.stdin
    print . L.sum . map energy3 $ iterate step3 moons0 !! 1000
    let solve c = fixp (\ms -> step1 ms <$> ms) (c <$> moons0)
    print $ solve V3.vX `lcm` solve V3.vY `lcm` solve V3.vZ
