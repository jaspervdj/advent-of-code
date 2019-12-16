{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Char           (digitToInt, isDigit)
import qualified Data.Vector.Unboxed as VU

ffft :: VU.Vector Int -> VU.Vector Int
ffft input = VU.generate (VU.length input) $ \outidx ->
    abs $ go (outidx + 1) 0 0 `rem` 10
  where
    go stride !acc i
        | i >= VU.length input = acc
        | otherwise            =
            let pos   = rangeSum (i + stride - 1)     stride
                neg   = rangeSum (i + stride * 3 - 1) stride in
            go stride (acc + pos - neg) (i + 4 * stride)

    prefixSums   = VU.scanl' (+) 0 input
    rangeSum i s = prefixSums VU.! clamp (i + s) - prefixSums VU.! clamp i
    clamp        = min (VU.length prefixSums - 1)

main :: IO ()
main = do
    start <- VU.fromList . map digitToInt . filter isDigit <$> getContents
    putStrLn . concatMap show . take 8 . VU.toList $ iterate ffft start !! 100
    let offset = VU.foldl (\acc x -> acc * 10 + x) 0 $ VU.take 7 start
        big    = VU.concatMap (const start) (VU.replicate 10000 ())
    putStrLn . concatMap show . VU.toList . VU.take 8 . VU.drop offset $
        iterate ffft big !! 100
