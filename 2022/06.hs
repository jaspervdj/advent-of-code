{-# LANGUAGE BangPatterns #-}
import           AdventOfCode.Main

import qualified Data.Map          as Map
import qualified Data.Sequence     as Seq

data Buffer a = Buffer
    { bMax   :: !Int
    , bFreqs :: !(Map.Map a Int)  -- Invariant: all strictly positive values
    , bRing  :: !(Seq.Seq a)
    } deriving (Show)

buffer :: Int -> Buffer a
buffer n = Buffer n Map.empty Seq.empty

pop :: Ord a => Buffer a -> Maybe (a, Buffer a)
pop b = case Seq.viewl (bRing b) of
    Seq.EmptyL -> Nothing
    x Seq.:< xs ->
        let freqs = Map.update
                (\f -> if f <= 1 then Nothing else Just (f - 1)) x (bFreqs b) in
        Just (x, b {bRing = xs, bFreqs = freqs})

push :: Ord a => a -> Buffer a -> Buffer a
push x b0 = b1
    { bRing  = bRing b1 Seq.|> x
    , bFreqs = Map.insertWith (+) x 1 $ bFreqs b1
    }
  where
    b1  | Seq.length (bRing b0) < bMax b0 = b0
        | otherwise                       = maybe b0 snd $ pop b0

sat :: Ord a => Buffer a -> Bool
sat b = bMax b == Map.size (bFreqs b)

solve :: Ord a => Buffer a -> [a] -> Int
solve = go 1
  where
    go !i _  []       = i
    go !i b0 (x : xs) =
        let b1 = push x b0 in
        if sat b1 then i else go (i + 1) b1 xs

main :: IO ()
main = simpleMain $ \input -> (solve (buffer 4) input, solve (buffer 14) input)
