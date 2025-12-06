import           AdventOfCode.Main (simpleMain)
import Data.Char (isSpace, isDigit)
import Data.List (transpose, groupBy)

charToOp :: Char -> [Int] -> Int
charToOp '*' = product
charToOp '+' = sum
charToOp c   = error $ "unknown operator: " ++ show c

part1 :: String -> Int
part1 = sum . map solve . transpose . map words . lines
  where
    solve col = charToOp (last (last col)) (map read (init col))

part2 :: String -> Int
part2 = sum . map solve . splitOnSpaces . transpose . map reverse . lines
  where
    splitOnSpaces =
        filter (not . all (all isSpace)) .
        groupBy (\x y -> not $ all isSpace x || all isSpace y)

    solve problem =
        charToOp (last (last problem)) $
        map read $
        map (filter isDigit) problem

main :: IO ()
main = simpleMain $ \str -> (part1 str, part2 str)
