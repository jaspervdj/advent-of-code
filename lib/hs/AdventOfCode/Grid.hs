-- | Simple 2D grids.
module AdventOfCode.Grid
    ( Dir (..)
    , turnLeft
    , turnRight
    , turnAround

    , Pos (..)
    , move
    , neighbours
    , diagonal

    , Grid
    , readGrid
    , center
    ) where

import           Control.Monad (foldM)
import qualified Data.Map      as M
import qualified System.IO     as IO

data Dir = U | R | D | L deriving (Bounded, Enum, Eq, Ord, Show)

turnRight, turnLeft, turnAround :: Dir -> Dir
turnRight  d = if d == maxBound then minBound else succ d
turnLeft   d = if d == minBound then maxBound else pred d
turnAround U = D
turnAround R = L
turnAround D = U
turnAround L = R

data Pos = Pos
    { pX :: {-# UNPACK #-} !Int
    , pY :: {-# UNPACK #-} !Int
    } deriving (Eq, Ord, Show)

-- | Up, down, left and right neighbours
neighbours :: Pos -> [Pos]
neighbours p = [move d p | d <- [minBound .. maxBound]]

-- | Diagonal neighbours
diagonal :: Pos -> [Pos]
diagonal (Pos x y) =
    [ Pos (x - 1) (y - 1)
    , Pos (x - 1) (y + 1)
    , Pos (x + 1) (y - 1)
    , Pos (x + 1) (y + 1)
    ]

move :: Dir -> Pos -> Pos
move dir (Pos x y) = case dir of
    U -> Pos x       (y - 1)
    L -> Pos (x - 1) y
    D -> Pos x       (y + 1)
    R -> Pos (x + 1) y

type Grid a = M.Map Pos a

readGrid :: (Char -> IO a) -> IO.Handle -> IO (Grid a)
readGrid f h = do
    ls <- lines <$> IO.hGetContents h
    foldM
        (\acc (y, l) -> foldM
            (\m (x, c) -> do
                v <- f c
                return $ M.insert (Pos x y) v m)
            acc
            (zip [0 ..] l))
        M.empty
        (zip [0 ..] ls)

center :: Grid a -> Pos
center grid = case M.maxViewWithKey grid of
    Nothing                -> error "center: Empty grid"
    Just ((Pos x y, _), _) -> Pos (x `div` 2) (y `div` 2)
