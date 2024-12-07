import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.Foldable           (toList)
import           Data.List.NonEmpty      (sort)

main :: IO ()
main = pureMain $ \input -> do
    inventory <- fmap sum <$> NP.runParser parseInventory input
    let part1 = maximum inventory :: Int
        part2 = sum . take 3 . reverse . toList $ sort inventory
    pure (pure part1, pure part2)
  where
    parseInventory = NP.sepBy1 (NP.many1 (NP.decimal <* NP.newline)) NP.newline
