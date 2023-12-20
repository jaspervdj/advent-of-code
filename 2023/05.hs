import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Ranges     as R
import           Data.List               (foldl')

data Input    = Input [(Int, Int)] [Mapping]     deriving (Show)
data RangeMap = RangeMap Int (R.Ranges Int)      deriving (Show)
data Mapping  = Mapping String String [RangeMap] deriving (Show)

mkRange :: Int -> Int -> R.Ranges Int
mkRange lo len = R.range lo (lo + len - 1)

parseInput :: NP.Parser Char Input
parseInput = Input <$> parseSeeds <*> NP.many1 parseMapping
  where
    parseSeeds    = NP.string "seeds:" *> NP.spaces *> NP.many1 parsePair
    parsePair     = (,) <$> num <*> num
    parseRangeMap = RangeMap <$> num <*> (uncurry mkRange <$> parsePair)
    parseMapping  = Mapping
        <$> ident <* NP.string "-to-"
        <*> ident <* NP.spaces <* NP.string "map:" <* NP.spaces
        <*> NP.many1 parseRangeMap

    num   = NP.decimal <* NP.spaces
    ident = NP.many1 NP.alpha

mapInt :: Mapping -> Int -> Int
mapInt (Mapping _ _ ranges0) x = go ranges0
  where
    go (RangeMap dst range : ranges)
        | R.member x range = dst + (x - R.minimum range)
        | otherwise        = go ranges
    go []                  = x

mapRange :: Mapping -> R.Ranges Int -> R.Ranges Int
mapRange (Mapping _ _ ranges0) input0 = mconcat $ go ranges0 [input0]
  where
    go [] inputs = inputs
    go _  []     = []
    go (RangeMap dst r1 : ranges) (input : inputs) =
        let overlap = R.intersection r1 input
            unmapped = R.difference input r1 in
        [R.offset (dst - R.minimum r1) overlap] ++
        go ranges ([unmapped | not (R.null unmapped)] ++ inputs)

main :: IO ()
main = pureMain $ \instr -> do
    Input ranges mappings <- NP.runParser parseInput instr
    let seeds1 = [s | (x, y) <- ranges, s <- [x, y]]
        part1 = map (\seed -> foldl' (flip mapInt) seed mappings) seeds1
        part2 = foldl'
            (\rs m -> mapRange m rs)
            (foldMap (uncurry mkRange) ranges)
            mappings
    pure (pure (minimum part1), pure (R.minimum part2))
