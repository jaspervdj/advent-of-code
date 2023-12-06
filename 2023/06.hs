import           AdventOfCode.Main
import qualified AdventOfCode.Parsing as Parsing

-- I want to avoid real numbers (sqrt) and stick with an approach that uses only
-- integral numbers.
--
-- The function that determines distance traveled is:
--
--     (time - hold) * hold
--
-- The derivative is:
--
--     time - 2 * hold
--
-- So the maximum is:
--
--     hold = time / 2
--
-- We can use this to bisect twice, once in [0 .. time / 2] and once in
-- [time / 2 .. 0].
race :: Int -> Int -> Int
race time record = upper - lower
  where
    distance hold = (time - hold) * hold

    lower = bisect (\hold -> distance hold > record) 0 (time `div` 2)
    upper = bisect (\hold -> distance hold <= record) (time `div` 2) time

    -- Find the first int for which f returns true in the given interval.
    bisect f lo hi
        | lo + 1 >= hi = hi
        | f mid        = bisect f lo mid
        | otherwise    = bisect f mid hi
      where
        mid = (lo + hi) `div` 2

main :: IO ()
main = simpleMain $ \str ->
    let [times, distances] = map Parsing.ints $ lines str

        part1 = product $ map (uncurry race) $ zip times distances
        glue  = read . concatMap show
        part2 = race (glue times) (glue distances) in
    (part1, part2)
