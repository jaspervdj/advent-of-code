{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Char           (digitToInt, isDigit)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

patterns :: [[Int]]
patterns = map (drop 1 . pattern) [1 ..]
  where
    pattern n = cycle $
        replicate n 0 ++ replicate n 1 ++
        replicate n 0 ++ replicate n (-1)

fft :: [Int] -> [Int]
fft input = map element (take (length input) patterns)
  where
    element pattern = abs . (`rem` 10) . sum $ zipWith (*) input pattern

ffft :: VU.Vector Int -> VU.Vector Int
ffft input = VU.generate (VU.length input) $ \outidx ->
    abs $ go (outidx + 1) 0 0 `rem` 10
  where
    go stride !acc i
        | i >= VU.length input = acc
        | otherwise            =
            let pos = VU.take stride $ VU.drop (i + stride - 1)     input
                neg = VU.take stride $ VU.drop (i + stride * 3 - 1) input in
            go stride (acc + VU.sum pos - VU.sum neg) (i + 4 * stride)

lfft :: V.Vector Int -> V.Vector Int
lfft input = V.generate (V.length input) $ \outidx ->
    abs $ go (outidx + 1) 0 0 `rem` 10
  where
    go stride !acc i
        | i >= V.length input = acc
        | otherwise            =
            let !pos = V.sum $ V.take stride $ V.drop (i + stride - 1)     input
                !neg = V.sum $ V.take stride $ V.drop (i + stride * 3 - 1) input in
            go stride (acc + pos - neg) (i + 4 * stride)

llfft :: Int -> V.Vector Int -> V.Vector (V.Vector Int)
llfft layers input =
    outputs
  where
    outputs = V.generate layers makeLayer

    makeLayer 0     = input
    makeLayer layer = V.generate (V.length input) $ \outidx ->
        abs $ go layer (outidx + 1) 0 0 `rem` 10

    go layer stride !acc i
        | i >= V.length input = acc
        | otherwise            =
            let !pos = V.sum $ V.take stride $ V.drop (i + stride - 1)     (outputs V.! (layer - 1))
                !neg = V.sum $ V.take stride $ V.drop (i + stride * 3 - 1) (outputs V.! (layer - 1)) in
            go layer stride (acc + pos - neg) (i + 4 * stride)

main :: IO ()
main = do
    start <- V.fromList . map digitToInt . filter isDigit <$> getContents
    putStrLn . concatMap show . take 8 . V.toList $ iterate lfft start !! 100

    let offset = V.foldl (\acc x -> acc * 10 + x) 0 $ V.take 7 start
        big    = V.concatMap (const start) (V.replicate 10000 ())
    putStrLn . concatMap show . V.toList . V.take 8 . V.drop offset $
        llfft 101 big V.! 100
