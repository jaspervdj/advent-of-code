import AdventOfCode.Main (simpleMain)
import Data.List         (sort, transpose)

main :: IO ()
main = simpleMain $ \input ->
    let [l1, l2] = transpose $ map (map read . words) $ lines input :: [[Int]]
        part1 = sum $ zipWith (\x y -> abs (x - y)) (sort l1) (sort l2)
        part2 = sum [x * length (filter (== x) l2) | x <- l1] in
    (part1, part2)
