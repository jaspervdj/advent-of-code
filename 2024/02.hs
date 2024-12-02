import AdventOfCode.Main (simpleMain)

type Level = Int

delta1to3 :: Level -> Level -> Bool
delta1to3 x y = let delta = abs (x - y) in delta >= 1 && delta <= 3

pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise f r = and $ zipWith f r (drop 1 r)

safe1 :: [Level] -> Bool
safe1 r = (pairwise (<) r || pairwise (>) r) && pairwise delta1to3 r

forget :: [a] -> [[a]]
forget []       = []
forget [x]      = [[]]
forget (x : xs) = xs : map (x :) (forget xs)

safe2 :: [Level] -> Bool
safe2 = any safe1 . forget

main :: IO ()
main = simpleMain $ \input ->
    let reports = map (map read . words) $ lines input in
    (length (filter safe1 reports), length (filter safe2 reports))
