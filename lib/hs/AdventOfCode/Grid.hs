-- | Simple 2D grids.
module AdventOfCode.Grid
    ( Dir (..)
    , turnLeft
    , turnRight
    , turnAround

    , Pos
    , move
    , neighbours
    , diagonal
    , manhattan

    , Grid
    , readGrid
    , printGrid
    , center
    , box
    ) where

import           AdventOfCode.V2
import qualified AdventOfCode.V2.Box as Box
import           Control.Monad       (foldM, forM_)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified System.IO           as IO

data Dir = U | R | D | L deriving (Bounded, Enum, Eq, Ord, Show)

turnRight, turnLeft, turnAround :: Dir -> Dir
turnRight  d = if d == maxBound then minBound else succ d
turnLeft   d = if d == minBound then maxBound else pred d
turnAround U = D
turnAround R = L
turnAround D = U
turnAround L = R

type Pos = V2 Int

-- | Up, down, left and right neighbours
neighbours :: Pos -> [Pos]
neighbours p = [move d p | d <- [minBound .. maxBound]]

-- | Diagonal neighbours
diagonal :: Pos -> [Pos]
diagonal (V2 x y) =
    [ V2 (x - 1) (y - 1)
    , V2 (x - 1) (y + 1)
    , V2 (x + 1) (y - 1)
    , V2 (x + 1) (y + 1)
    ]

manhattan :: Pos -> Pos -> Int
manhattan (V2 lx ly) (V2 rx ry) = abs (lx - rx) + abs (ly - ry)

move :: Dir -> Pos -> Pos
move dir (V2 x y) = case dir of
    U -> V2 x       (y - 1)
    L -> V2 (x - 1) y
    D -> V2 x       (y + 1)
    R -> V2 (x + 1) y

type Grid a = M.Map Pos a

readGrid :: (Char -> IO a) -> IO.Handle -> IO (Grid a)
readGrid f h = do
    ls <- lines <$> IO.hGetContents h
    foldM
        (\acc (y, l) -> foldM
            (\m (x, c) -> do
                v <- f c
                return $ M.insert (V2 x y) v m)
            acc
            (zip [0 ..] l))
        M.empty
        (zip [0 ..] ls)

printGrid :: IO.Handle -> Grid Char -> IO ()
printGrid h grid = case box grid of
    Nothing -> IO.hPutStrLn h "<empty grid>"
    Just (Box.Box (V2 minX minY) (V2 maxX maxY)) ->
        forM_ [minY .. maxY] $ \y -> IO.hPutStrLn h
            [fromMaybe ' ' (M.lookup (V2 x y) grid)  | x <- [minX .. maxX]]

center :: Grid a -> Pos
center grid = case M.maxViewWithKey grid of
    Nothing               -> error "center: Empty grid"
    Just ((V2 x y, _), _) -> V2 (x `div` 2) (y `div` 2)

box :: Grid a -> Maybe (Box.Box Int)
box grid
    | M.null grid = Nothing
    | otherwise   = Just $ L.foldl1' (<>) $ map Box.fromV2 $ M.keys grid
