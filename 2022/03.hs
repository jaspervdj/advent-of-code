import           AdventOfCode.Main
import           Data.Char         (isLower, ord)

priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

group3 :: [a] -> [(a, a, a)]
group3 (x : y : z : t) = (x, y, z) : group3 t
group3 _               = []

main :: IO ()
main = simpleMain $ \input ->
    let rucksacks = lines input
        part1 = sum $ do
            rucksack <- rucksacks
            let (cpt1, cpt2) = splitAt (length rucksack `div` 2) rucksack
                wrong = filter (`elem` cpt2) cpt1
            pure . priority $ head wrong
        part2 = sum $ do
            (x, y, z) <- group3 rucksacks
            let common = filter (`elem` z) $ filter (`elem` y) x
            pure . priority $ head common in
    (part1, part2)
