-- | Simple 2D grids.
module AdventOfCode.Grid
    ( Dir (..)
    , turnLeft
    , turnRight
    , turnAround

    , Pos
    , origin
    , move
    , neighbours
    , diagonal
    , manhattan

    , Grid
    , fromList
    , fromString
    , readGrid
    , toString
    , printGrid
    , center
    , box
    ) where

import           AdventOfCode.V2
import qualified AdventOfCode.V2.Box as Box
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

origin :: Pos
origin = zero

-- | Up, down, left and right neighbours
neighbours :: Pos -> [Pos]
neighbours (V2 x y) =
    [V2 x (y - 1), V2 (x + 1) y, V2 x (y + 1) , V2 (x - 1) y]

-- | Diagonal neighbours
diagonal :: Pos -> [Pos]
diagonal (V2 x y) =
    [ V2 (x - 1) (y - 1), V2 (x - 1) (y + 1)
    , V2 (x + 1) (y - 1), V2 (x + 1) (y + 1)
    ]

manhattan :: Pos -> Pos -> Int
manhattan (V2 lx ly) (V2 rx ry) = abs (lx - rx) + abs (ly - ry)

move :: Int -> Dir -> Pos -> Pos
move n dir (V2 x y) = case dir of
    U -> V2 x       (y - n)
    L -> V2 (x - n) y
    D -> V2 x       (y + n)
    R -> V2 (x + n) y

type Grid a = M.Map Pos a

fromList :: [[a]] -> Grid a
fromList = L.foldl'
    (\acc (y, row) -> L.foldl'
        (\m (x, c) -> M.insert (V2 x y) c m) acc (zip [0 ..] row))
    M.empty .
    zip [0 ..]

fromString :: String -> Grid Char
fromString = fromList . lines

toString :: Grid Char -> String
toString grid = case box grid of
    Nothing -> "<empty grid>"
    Just (Box.Box (V2 minX minY) (V2 maxX maxY)) -> unlines $ do
        y <- [minY .. maxY]
        pure [fromMaybe ' ' (M.lookup (V2 x y) grid)  | x <- [minX .. maxX]]

readGrid :: (Char -> IO a) -> IO.Handle -> IO (Grid a)
readGrid f h = IO.hGetContents h >>= traverse f . fromString

printGrid :: IO.Handle -> Grid Char -> IO ()
printGrid h = IO.hPutStrLn h . toString

center :: Grid a -> Maybe Pos
center grid = case M.maxViewWithKey grid of
    Nothing               -> Nothing
    Just ((V2 x y, _), _) -> Just $ V2 (x `div` 2) (y `div` 2)

box :: Grid a -> Maybe (Box.Box Int)
box grid
    | M.null grid = Nothing
    | otherwise   = Just $ L.foldl1' (<>) $ map Box.fromV2 $ M.keys grid
