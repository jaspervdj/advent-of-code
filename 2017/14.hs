{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified AdventOfCode.KnotHash       as KnotHash
import           Control.Monad               (unless)
import           Control.Monad.ST            (runST)
import           Data.Bits                   (FiniteBits, finiteBitSize,
                                              testBit)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Prelude                     hiding (round)

toBits :: FiniteBits a => a -> VU.Vector Bool
toBits x =
    let !s = finiteBitSize x in
    VU.generate s $ \idx -> testBit x (s - 1 - idx)

data Grid a = Grid {gRows :: Int, gCols :: Int, gCells :: VU.Vector a}

positionsInGrid :: Grid a -> [Pos]
positionsInGrid g =
    [Pos r c | r <- [0 .. gRows g - 1], c <- [0 .. gCols g - 1]]

positionInGrid :: Pos -> Grid a -> Bool
positionInGrid (Pos r c) g = r >= 0 && r < gRows g && c >= 0 && c < gCols g

indexGrid :: VU.Unbox a => Pos -> Grid a -> a
indexGrid pos grid = gCells grid VU.! posToIndex pos grid

data Pos = Pos !Int !Int deriving (Show)

posToIndex :: Pos -> Grid a -> Int
posToIndex (Pos r c) g = r * gCols g + c

posNeighbours :: Pos -> [Pos]
posNeighbours (Pos r c) =
    [Pos (r - 1) c, Pos r (c + 1), Pos (r + 1) c, Pos r (c - 1)]

hashGrid :: String -> Grid Bool
hashGrid input = Grid 128 128 $ VU.concat $ do
    row <- [0 .. 127 :: Int]
    let hash = KnotHash.knotHash (input ++ "-" ++ show row)
    return $ VU.concatMap toBits hash

problem01 :: Grid Bool -> Int
problem01 grid = length
    [() | pos <- positionsInGrid grid, indexGrid pos grid]

problem02 :: Grid Bool -> Int
problem02 g = runST $ do
    visited <- VUM.replicate (gRows g * gCols g) False

    let markRegion pos = do
            let !idx = posToIndex pos g
            alreadyBeenHere <- VUM.read visited idx
            unless alreadyBeenHere $ do
                VUM.write visited idx True
                mapM_ markRegion
                    [ p
                    | p <- posNeighbours pos
                    , positionInGrid p g
                    , indexGrid p g
                    ]

        countRegions !acc []            = return acc
        countRegions !acc (pos : queue) = do
            let !idx = posToIndex pos g
            alreadyBeenHere <- VUM.read visited idx
            if alreadyBeenHere || not (indexGrid pos g)
                then countRegions acc queue
                else do
                    markRegion pos
                    countRegions (acc + 1) queue

    countRegions 0 $ positionsInGrid g

main :: IO ()
main = do
    input <- getLine
    let grid = hashGrid input
    putStrLn $ "Num squares used: " ++ show (problem01 grid)
    putStrLn $ "Region count: " ++ show (problem02 grid)
