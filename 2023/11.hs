{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main
import           AdventOfCode.V2   (V2 (..))
import qualified Data.Map          as M

-- | 'expandWith' takes two functions to pack and unpack points to and from
-- primary and secondary components.  The expansion only happens for the
-- primary components.
expandWith :: (a -> (Int, s)) -> ((Int, s) -> a) -> Int -> [a] -> [a]
expandWith unpack pack factor = ungroup . go 0 . group
  where
    ungroup l = [pack (p, s) | (p, ss) <- l, s <- ss]
    group   l = M.toAscList $ M.fromListWith (++) $ map (fmap pure . unpack) l

    go :: Int -> [(Int, [s])] -> [(Int, [s])]
    go !_      []            = []
    go !offset ((p0, s) : t) = (p0 + offset, s) : case t of
        []            -> []
        ((p1, _) : _) -> go (offset + (p1 - p0 - 1) * (factor - 1)) t

expand :: Int -> [V2 Int] -> [V2 Int]
expand factor =
    expandWith (\(V2 x y) -> (x, y)) (\(x, y) -> V2 x y) factor .
    expandWith (\(V2 x y) -> (y, x)) (\(y, x) -> V2 x y) factor

solve :: Int -> [V2 Int] -> Int
solve factor galaxies =
    let expanded = expand factor galaxies in
    sum [G.manhattan x y | x <- expanded, y <- expanded, x > y]

main :: IO ()
main = simpleMain $ \str ->
    let galaxies = M.keys . M.filter (== '#') $ G.fromString str in
    (solve 2 galaxies, solve 1000000 galaxies)
