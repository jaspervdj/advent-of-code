import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.List               (foldl')
import           Data.Maybe              (mapMaybe)

--------------------------------------------------------------------------------

data RangeSet
    = RSCons {-# UNPACK #-} !Int {-# UNPACK #-} !Int !RangeSet
    | RSNil

empty :: RangeSet
empty = RSNil

insert :: Int -> Int -> RangeSet -> RangeSet
insert l h RSNil = RSCons l h RSNil
insert l0 h0 rs@(RSCons l1 h1 rss)
    | h0 + 1 < l1 = RSCons l0 h0 rs
    | l0 > h1 + 1 = RSCons l1 h1 (insert l0 h0 rss)
    | otherwise   = insert (min l0 l1) (max h0 h1) rss

holes :: RangeSet -> RangeSet
holes (RSCons _ h0 rs@(RSCons l1 _ _)) = RSCons (h0 + 1) (l1 - 1) (holes rs)
holes _                                = RSNil

size :: RangeSet -> Int
size (RSCons l h rs) = h - l + 1 + size rs
size RSNil           = 0

toList :: RangeSet -> [Int]
toList (RSCons l h rs) = [l .. h] ++ toList rs
toList RSNil           = []

--------------------------------------------------------------------------------

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int

manhattan :: Pos -> Pos -> Int
manhattan (Pos x0 y0) (Pos x1 y1) = abs (x1 - x0) + abs (y1 - y0)

--------------------------------------------------------------------------------

data Sensor = Sensor !Pos !Pos

parseSensors :: NP.Parser Char [Sensor]
parseSensors = NP.sepBy1 pair NP.newline
  where
    pair = Sensor
        <$> (NP.string "Sensor at " *> pos)
        <*> (NP.string ": closest beacon is at " *> pos)
    pos = Pos
        <$> (NP.string "x=" *> NP.signedDecimal)
        <*> (NP.string ", y=" *> NP.signedDecimal)

coverRow :: Int -> Sensor -> Maybe (Int, Int)
coverRow y (Sensor sensor@(Pos sx sy) beacon)
    | dy > dist = Nothing
    | otherwise = Just (sx - dist + dy, sx + dist - dy)
  where
    dy   = abs $ y - sy
    dist = manhattan sensor beacon

part1 :: Int -> [Sensor] -> Int
part1 row =
    size . foldl' (\rs (lo, hi) -> insert lo hi rs) empty .
    mapMaybe (coverRow row)

part2 :: Int -> [Sensor] -> Int
part2 bound sensors = head $ do
    y <- [0 .. bound]
    x <- toList . holes $
        foldl' (\rs (lo, hi) -> insert lo hi rs) empty $
        mapMaybe (coverRow y) sensors
    pure $ x * bound + y

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseSensors str
    pure (pure (part1 2000000 input), pure (part2 4000000 input))
