-- | Utilities for dealing with hexagonal grids.
module AdventOfCode.Hex
    ( Cubic (..)
    , zero
    , distance

    , Dir (..)
    , move
    ) where

-- | We only care about calculating distances, so it is really easiest to treat
-- the hexagonal 2D grid as a 3D space.
data Cubic = Cubic
    { cX :: {-# UNPACK #-} !Int
    , cY :: {-# UNPACK #-} !Int
    , cZ :: {-# UNPACK #-} !Int
    } deriving (Eq, Show)

-- | Origin.
zero :: Cubic
zero = Cubic 0 0 0

-- | Distance between two points in the 3D space.
distance :: Cubic -> Cubic -> Int
distance (Cubic x1 y1 z1) (Cubic x2 y2 z2) =
    let !dx = abs (x2 - x1)
        !dy = abs (y2 - y1)
        !dz = abs (z2 - z1) in
    max dx (max dy dz)
    -- dx + dy + dz

-- | Directions we can move in.
data Dir
    = N
    | NE
    | SE
    | S
    | SW
    | NW
    deriving (Eq, Ord, Show)

-- | Move a point a single step into a direction
move :: Dir -> Cubic -> Cubic
move dir (Cubic x y z) = case dir of
    N  -> Cubic (x - 1) y       (z + 1)
    NE -> Cubic x       (y - 1) (z + 1)
    SE -> Cubic (x + 1) (y - 1) z
    S  -> Cubic (x + 1) y       (z - 1)
    SW -> Cubic x       (y + 1) (z - 1)
    NW -> Cubic (x - 1) (y + 1) z

