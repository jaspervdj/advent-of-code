import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.List               (foldl')
import           Data.Maybe              (catMaybes, maybeToList)

data Input    = Input [Range] [Mapping]          deriving (Show)
data Range    = Range Int Int                    deriving (Show)
data RangeMap = RangeMap Int Range               deriving (Show)
data Mapping  = Mapping String String [RangeMap] deriving (Show)

parseInput :: NP.Parser Char Input
parseInput = Input <$> parseSeeds <*> NP.many1 parseMapping
  where
    parseSeeds    = NP.string "seeds:" *> NP.spaces *> NP.many1 parseRange
    parseRange    = Range <$> num <*> num
    parseRangeMap = RangeMap <$> num <*> parseRange
    parseMapping  = Mapping
        <$> ident <* NP.string "-to-"
        <*> ident <* NP.spaces <* NP.string "map:" <* NP.spaces
        <*> NP.many1 parseRangeMap

    num   = NP.decimal <* NP.spaces
    ident = NP.many1 NP.alpha

mapInt :: Mapping -> Int -> Int
mapInt (Mapping _ _ ranges0) x = go ranges0
  where
    go (RangeMap dst (Range src len) : ranges)
        | x >= src && x < src + len = dst + (x - src)
        | otherwise                 = go ranges
    go []                           = x

-- Cuts the second range into (before part, intersection part, after part)
partitionRange :: Range -> Range -> (Maybe Range, Maybe Range, Maybe Range)
partitionRange (Range lo0 len0) r1@(Range lo1 len1)
    | hi1 < lo0 = (Just r1, Nothing, Nothing)
    | lo1 > hi0 = (Nothing, Nothing, Just r1)
    | lo1 >= lo0 && hi1 <= hi0 = (Nothing, Just r1, Nothing)
    | hi1 < hi0 = (mk lo1 (lo0 - 1), mk lo0 hi1, Nothing)
    | lo1 > lo0 = (Nothing, mk lo1 hi0, mk (hi0 + 1) hi1)
    | otherwise = (mk lo0 (lo0 - 1), mk lo0 hi0, mk (hi0 + 1) hi1)
  where
    hi0      = lo0 + len0 - 1
    hi1      = lo1 + len1 - 1
    mk lo hi = if lo >= hi then Nothing else pure $ Range lo (hi - lo + 1)

offsetRange :: Int -> Range -> Range
offsetRange delta (Range lo len) = Range (lo + delta) len

mapRange :: Mapping -> Range -> [Range]
mapRange (Mapping _ _ ranges0) input0 = go ranges0 [input0]
  where
    go [] inputs = inputs
    go _  []     = []
    go (RangeMap dst r1@(Range src _) : ranges) (input : inputs) =
        let (pre, overlap, post) = partitionRange r1 input in
        maybeToList (offsetRange (dst - src) <$> overlap) ++
        go ranges (catMaybes [pre, post] ++ inputs)

main :: IO ()
main = pureMain $ \instr -> do
    Input ranges mappings <- NP.runParser parseInput instr
    let seeds1 = [s | Range x y <- ranges, s <- [x, y]]
        part1 = map (\seed -> foldl' (flip mapInt) seed mappings) seeds1
        part2 = foldl' (\rs m -> concatMap (mapRange m) rs) ranges mappings
    pure (pure (minimum part1), pure (minimum [l | Range l _ <- part2]))
