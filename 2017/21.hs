{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Monad         (foldM)
import qualified Data.Bits             as Bits
import qualified Data.ByteString.Char8 as BC8
import qualified Data.IntMap           as IM
import           Data.List             (foldl')
import qualified Data.Vector           as V
import qualified System.IO             as IO

--------------------------------------------------------------------------------

-- | A square grid.
class Grid a where
    type Cell a :: *

    size     :: a -> Int
    index    :: Int -> Int -> a -> Cell a
    generate :: Int -> (Int -> Int -> Cell a) -> a

-- | All flips and rotations of a grid.  May contain duplicates.
variations :: Grid a => a -> [a]
variations s0 =
    let !s1 = rotateClockwise s0
        !s2 = rotateClockwise s1
        !s3 = rotateClockwise s2 in
    [f | s <- [s0, s1, s2, s3], f <- [s, flipHorizontal s, flipVertical s]]
  where
    rotateClockwise, flipHorizontal, flipVertical :: Grid a => a -> a
    rotateClockwise grid =
        generate (size grid) $ \x y -> index y (size grid - 1 - x) grid
    flipHorizontal  grid =
        generate (size grid) $ \x y -> index (size grid - 1 - x) y grid
    flipVertical   grid =
        generate (size grid) $ \x y -> index x (size grid - 1 - y) grid

-- | Parse a grid from ".#./..#/###" notation.
parseGrid :: (Grid a, Cell a ~ Bool) => BC8.ByteString -> a
parseGrid bytes =
    let !s = floor (sqrt (fromIntegral (BC8.length bytes)) :: Double) in
    generate s (\x y ->
        let !idx = y * (s + 1) + x in BC8.index bytes idx == '#')

-- | Print a grid in human-readable notation.
showGrid :: (Grid a, Cell a ~ Bool) => a -> String
showGrid grid = unlines
    [ [if index x y grid then '#' else '.' | x <- [0 .. size grid - 1]]
    | y <- [0 .. size grid - 1]
    ]

-- | Count the number of 'True's in a boolean grid.
count :: (Grid a, Cell a ~ Bool) => a -> Int
count grid = length $ filter id
    [index x y grid | y <- [0 .. size grid - 1], x <- [0 .. size grid - 1]]

--------------------------------------------------------------------------------

-- | Subdivide a big grid into a grid of grids.
subdivide :: (Grid s, Grid t, Grid (Cell t), Cell s ~ Cell (Cell t)) => s -> t
subdivide big =
    let !smallSize = if size big `mod` 2 == 0 then 2 else 3
        !smallNum  = size big `div` smallSize in
    generate smallNum $ \col row ->
        generate smallSize $ \x y ->
            index (col * smallSize + x) (row * smallSize + y) big

-- | Combine a grid of grids back into a big grid.
combine :: (Grid s, Grid t, Grid (Cell t), Cell s ~ Cell (Cell t)) => t -> s
combine smalls =
    let !smallSize = size (index 0 0 smalls) in
    generate (size smalls * smallSize) $ \x y ->
        let !(col, x') = x `divMod` smallSize
            !(row, y') = y `divMod` smallSize in
        index x' y' (index col row smalls)

-- | First subdivide the grid, then perform a transformation on the subgrids and
-- combine them again.
iteration
    :: (Grid s, Grid t, Grid (Cell t), Cell s ~ Cell (Cell t))
    => (t -> t) -> s -> s
iteration f = combine . f . subdivide

--------------------------------------------------------------------------------

-- | A very compact representation of a grid.  Can only handle grids of size up
-- to 4.
newtype Compact = Compact Int deriving (Eq, Ord, Show)

instance Grid Compact where
    type Cell Compact = Bool

    size :: Compact -> Int
    size (Compact bits) = bits `Bits.shiftR` 16

    index :: Int -> Int -> Compact -> Bool
    index x y grid@(Compact bits) = Bits.testBit bits ((size grid * y) + x)

    generate :: Int -> (Int -> Int -> Bool) -> Compact
    generate s f
        | s > 4     = error "Cannot create compact grid of size > 4"
        | otherwise = Compact $ foldl'
            (\grid y ->
                foldl'
                    (\bits x ->
                        if f x y then Bits.setBit bits (s * y + x) else bits)
                    grid
                    [0 .. s - 1])
            (s `Bits.shiftL` 16)
            [0 .. s - 1]

--------------------------------------------------------------------------------

-- | A more flexible representation of a grid which has the very useful functor
-- instance.
data Flex a = Flex !Int !(V.Vector a) deriving (Eq, Functor, Ord, Show)

instance Grid (Flex a) where
    type Cell (Flex a) = a

    size (Flex s _)      = s
    index x y (Flex s v) = v V.! (y * s + x)
    generate s f         = Flex s
        (V.generate (s * s) $ \i -> let (y, x) = i `divMod` s in f x y)

--------------------------------------------------------------------------------

-- | A list of rules for transformations.
newtype RuleBook = RuleBook (IM.IntMap Compact) deriving (Show)

-- | Nasty parser.
readRuleBook :: IO.Handle -> IO RuleBook
readRuleBook h = do
    ls    <- BC8.lines <$> BC8.hGetContents h
    rules <- mapM parseRule ls
    foldM insertRule (RuleBook IM.empty)
        [(lhs', rhs) | (lhs, rhs) <- rules , lhs' <- variations lhs]
  where
    parseRule :: BC8.ByteString -> IO (Compact, Compact)
    parseRule l = case BC8.words l of
        [lhs, "=>", rhs] -> return (parseGrid lhs, parseGrid rhs)
        _                -> fail $ "Could not parse rule: " ++ show l

    insertRule :: RuleBook -> (Compact, Compact) -> IO RuleBook
    insertRule (RuleBook m) (Compact lhs, rhs) = case IM.lookup lhs m of
        Nothing   -> return (RuleBook (IM.insert lhs rhs m))
        Just rhs' | rhs' /= rhs -> fail "Conflicting rule!"
        Just _    -> return (RuleBook m)

-- | Transform a compact grid according to the rulebook.
(.=>) :: RuleBook -> Compact -> Compact
RuleBook ruleBook .=> Compact lhs = ruleBook IM.! lhs

--------------------------------------------------------------------------------

-- | Main function.
main :: IO ()
main = do
    ruleBook <- readRuleBook IO.stdin
    let grid0     = parseGrid ".#./..#/###" :: Flex Bool
        transform = fmap (ruleBook .=>) :: Flex Compact -> Flex Compact
        grids     = iterate (iteration transform) grid0

    putStrLn $ showGrid grid0
    putStrLn $ "After 5 iterations: "  ++ show (count (grids !! 5))
    putStrLn $ "After 18 iterations: " ++ show (count (grids !! 18))
