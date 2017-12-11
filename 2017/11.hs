{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC8
import           Data.Char             (isSpace)
import           Data.List             (foldl')

-- | We only care about calculating distances, so it is really easiest to treat
-- the hexagonal 2D grid as a 3D space.
data V3 = V3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Show)

-- | Origin.
zero :: V3
zero = V3 0 0 0

-- | Distance between two points in the 3D space.
distance :: V3 -> V3 -> Int
distance (V3 x1 y1 z1) (V3 x2 y2 z2) =
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
move :: Dir -> V3 -> V3
move dir (V3 x y z) = case dir of
    N  -> V3 (x - 1) y       (z + 1)
    NE -> V3 x       (y - 1) (z + 1)
    SE -> V3 (x + 1) (y - 1) z
    S  -> V3 (x + 1) y       (z - 1)
    SW -> V3 x       (y + 1) (z - 1)
    NW -> V3 (x - 1) (y + 1) z

-- | Follow a path.  Also return the furthest distance from the start we
-- encountered.
path :: [Dir] -> V3 -> (V3, Int)
path dirs origin = foldl'
    (\(!p0, !maxdist0) dir ->
        let !p1       = move dir p0
            !maxdist1 = max maxdist0 (distance p1 origin) in
        (p1, maxdist1))
    (origin, 0)
    dirs

-- | Parse directions
parse :: BC8.ByteString -> [Dir]
parse input =
    [x | d <- BC8.split ',' (BC8.filter (not . isSpace) input), x <- parseDir d]
  where
    parseDir :: BC8.ByteString -> [Dir]
    parseDir "n"  = [N]
    parseDir "ne" = [NE]
    parseDir "se" = [SE]
    parseDir "s"  = [S]
    parseDir "sw" = [SW]
    parseDir "nw" = [NW]
    parseDir _    = []

main :: IO ()
main = do
    input <- BC8.getContents
    let (endpos, maxdist) = path (parse input) zero
    putStrLn $ "Distance to final position: " ++ show (distance zero endpos)
    putStrLn $ "Distance to furthest position: " ++ show maxdist
