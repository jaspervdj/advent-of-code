import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)

type Range a = (a, a)

rangeFullyContains :: Ord a => Range a -> Range a -> Bool
rangeFullyContains (lo, hi) (x, y) = x >= lo && y <= hi

rangeOverlaps :: Ord a => Range a -> Range a -> Bool
rangeOverlaps (lo0, hi0) (lo1, hi1) = not $
    hi0 < lo1 || hi1 < lo0 || lo0 > hi1 || lo1 > hi0

type Assignment a = (Range a, Range a)

assignmentFullyContains :: Ord a => Assignment a -> Bool
assignmentFullyContains (x, y) = rangeFullyContains x y || rangeFullyContains y x

assignmentOverlaps :: Ord a => Assignment a -> Bool
assignmentOverlaps = uncurry rangeOverlaps

parseAssignments :: NP.Parser Char [Assignment Int]
parseAssignments = many (parseAssignment <* NP.newline)
  where
    parseAssignment = (,) <$> (parseRange <* NP.char ',') <*> parseRange
    parseRange      = (,) <$> (NP.decimal <* NP.char '-') <*> NP.decimal

main :: IO ()
main = pureMain $ \input -> do
    assignments <- NP.runParser parseAssignments input
    let part1 = length $ filter assignmentFullyContains assignments
        part2 = length $ filter assignmentOverlaps assignments
    pure (pure part1, pure part2)
