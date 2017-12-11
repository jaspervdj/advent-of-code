{-# LANGUAGE BangPatterns #-}
import           Data.Bits ((.&.))
import           Data.Word (Word64)
import qualified System.IO as IO

type Generator = [Word64]

mkGenerator :: Word64 -> Word64 -> [Word64]
mkGenerator factor = go
  where
    go x = let !y = (x * factor) `mod` 2147483647 in y : go y
{-# NOINLINE mkGenerator #-}

mkGenerators :: (Word64, Word64) -> (Generator, Generator)
mkGenerators (a, b) = (mkGenerator 16807 a, mkGenerator 48271 b)

dirtyParse :: IO.Handle -> IO (Word64, Word64)
dirtyParse h = do
    [lineA, lineB] <- lines <$> IO.hGetContents h
    let getNum = read . last . words
    return (getNum lineA, getNum lineB)

match16 :: Word64 -> Word64 -> Bool
match16 x y = x .&. 0xFFFF == y .&. 0xFFFF

problem01 :: (Word64, Word64) -> Int
problem01 seeds =
    let (a, b) = mkGenerators seeds in
    length $ filter id $ take (40 * 1000 * 1000) $ zipWith match16 a b

problem02 :: (Word64, Word64) -> Int
problem02 seeds =
    let (a, b) = mkGenerators seeds
        a'     = filter (\x -> x .&. 0x3 == 0) a
        b'     = filter (\x -> x .&. 0x7 == 0) b in
    length $ filter id $ take (5 * 1000 * 1000) $ zipWith match16 a' b'

main :: IO ()
main = do
    seeds <- dirtyParse IO.stdin
    putStrLn $ "Num matching pairs (1): " ++ show (problem01 seeds)
    putStrLn $ "Num matching pairs (2): " ++ show (problem02 seeds)
