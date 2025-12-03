import           AdventOfCode.Main (simpleMain)
import           Data.Char         (digitToInt)
import           Data.Int          (Int64)
import qualified Data.Vector       as V

pickN :: Integral a => Int -> V.Vector a -> a
pickN = go 0
  where
    go !acc 0 _ = acc
    go !acc n v =
        let i0 = V.maxIndex $ V.take (V.length v - n + 1) v in
        go (acc * 10 + v V.! i0) (n - 1) $ V.drop (i0 + 1) v

main :: IO ()
main = simpleMain $ \str ->
    let banks = map (V.fromList . map (fromIntegral . digitToInt)) $ lines str
        part1 = sum $ map (pickN 2) banks :: Int64
        part2 = sum $ map (pickN 12) banks :: Int64 in
    (part1, part2)
