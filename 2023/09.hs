import           AdventOfCode.Main
import qualified AdventOfCode.Parsing as Parsing

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

predict :: [Int] -> Int
predict = sum . map last . takeWhile (any (/= 0)) . iterate diffs

main :: IO ()
main = simpleMain $ \str ->
    let histories = map Parsing.ints $ lines str in
    (sum (map predict histories), sum (map (predict . reverse) histories))
