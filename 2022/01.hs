import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.List               (sort)

main :: IO ()
main = pureMain $ \input -> do
    inventory <- map sum <$> NP.runParser parseInventory input
    let part1 = maximum inventory :: Int
        part2 = sum . take 3 . reverse $ sort inventory
    pure (pure part1, pure part2)
  where
    parseInventory = NP.sepBy1 (NP.many1 (NP.decimal <* NP.newline)) NP.newline
