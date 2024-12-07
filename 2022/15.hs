import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Ranges     as R
import           Data.Foldable           (foldl', toList)
import           Data.Maybe              (mapMaybe)

--------------------------------------------------------------------------------

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int

manhattan :: Pos -> Pos -> Int
manhattan (Pos x0 y0) (Pos x1 y1) = abs (x1 - x0) + abs (y1 - y0)

--------------------------------------------------------------------------------

data Sensor = Sensor !Pos !Pos

parseSensors :: NP.Parser Char [Sensor]
parseSensors = toList <$> NP.sepBy1 pair NP.newline
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
    R.size . foldl' (\rs (lo, hi) -> R.range lo hi <> rs) mempty .
    mapMaybe (coverRow row)

holes :: R.Ranges Int -> R.Ranges Int
holes ranges = mconcat $
    zipWith (\(_, h) (l, _) -> R.range (h + 1) (l - 1)) list (drop 1 list)
  where
    list = R.toPairs ranges

part2 :: Int -> [Sensor] -> Int
part2 bound sensors = head $ do
    y <- [0 .. bound]
    x <- R.toList . holes $
        foldl' (\rs (lo, hi) -> R.range lo hi <> rs) mempty $
        mapMaybe (coverRow y) sensors
    pure $ x * bound + y

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseSensors str
    pure (pure (part1 2000000 input), pure (part2 4000000 input))
