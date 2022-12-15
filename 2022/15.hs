import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import           Data.List               (foldl', sortOn)
import           Data.Maybe              (fromJust, isJust)
import qualified Data.Set                as S

--------------------------------------------------------------------------------

data Range a = Range a a deriving (Show)

newtype Ranges a = Ranges [Range a] deriving (Show)

mergeRange :: Integral a => Range a -> Range a -> Maybe (Range a)
mergeRange (Range lo0 hi0) (Range lo1 hi1)
    | hi0 + 1 < lo1 || lo0 - 1 > hi1 = Nothing
    | otherwise                      = Just $ Range (min lo0 lo1) (max hi0 hi1)

insertRange :: Integral a => Range a -> Ranges a -> Ranges a
insertRange r (Ranges rs) = case break (isJust . mergeRange r) rs of
    (_,   [])         -> Ranges (r : rs)
    (pre, (x : post)) ->
        insertRange (fromJust (mergeRange r x)) (Ranges $ pre ++ post)

singleton :: Integral a => a -> a -> Ranges a
singleton x y = Ranges [if x > y then Range y x else Range x y]

toList :: Integral a => Ranges a -> [a]
toList (Ranges rs) = [x | Range lo hi <- rs, x <- [lo .. hi]]

toSet :: Integral a => Ranges a -> S.Set a
toSet = S.fromList . toList

intersect :: Integral a => Range a -> Ranges a -> Ranges a
intersect bound (Ranges rs) = Ranges [r | r <- rs, isJust (mergeRange bound r)]

complement :: Integral a => Ranges a -> Ranges a
complement (Ranges rs0) = Ranges $ go (sortOn (\(Range l _) -> l) rs0)
  where
    go (Range _ hi0 : Range lo1 hi1 : rs) =
        Range (hi0 + 1) (lo1 - 1) : go (Range lo1 hi1 : rs)
    go _ = []

instance Integral a => Semigroup (Ranges a) where
    acc <> Ranges rs = foldl' (flip insertRange) acc rs

instance Integral a => Monoid (Ranges a) where
    mempty  = Ranges []
    mappend = (<>)

--------------------------------------------------------------------------------

type Sensor = (G.Pos, G.Pos)

parseSensors :: NP.Parser Char [Sensor]
parseSensors = NP.sepBy1 pair NP.newline
  where
    pair = (,) <$>
        (NP.string "Sensor at " *> pos) <*>
        (NP.string ": closest beacon is at " *> pos)
    pos = V2
        <$> (NP.string "x=" *> NP.signedDecimal)
        <*> (NP.string ", y=" *> NP.signedDecimal)

coverRow :: Int -> Sensor -> Ranges Int
coverRow y (sensor@(V2 sx sy), beacon)
    | dy > manhattan = mempty
    | otherwise      = singleton (sx - manhattan + dy) (sx + manhattan - dy)
  where
    dy = abs $ y - sy
    manhattan = G.manhattan sensor beacon

part1 :: Int -> [Sensor] -> Int
part1 row input =
    S.size . (`S.difference` beacons) . toSet $ foldMap (coverRow row) input
  where
    beacons = S.fromList [x | (_, V2 x y) <- input, y == row]

part2 :: Int -> [Sensor] -> Int
part2 bound sensors = head $ do
    y <- [0 .. bound]
    x <- toList . complement . intersect (Range 0 bound) $
        foldMap (coverRow y) sensors
    pure $ x * bound + y

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseSensors str
    pure (pure (part1 2000000 input), pure (show (part2 4000000 input)))
