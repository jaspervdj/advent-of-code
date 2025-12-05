import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import qualified Data.IntMap             as IM
import           Data.List               (foldl')

data Ingredients = Ingredients [(Int, Int)] [Int] deriving (Show)

parseIngredients :: NP.Parser Char Ingredients
parseIngredients = Ingredients
    <$> many (parseRange <* NP.newline)
    <*  NP.newline
    <*> many (NP.decimal <* NP.spaces)
  where
    parseRange = (,) <$> NP.decimal <*  NP.char '-' <*> NP.decimal

-- | We (ab)use the fact that IntMap is a sorted container.  We store the ranges
-- ordered by the lower end.
type Ranges = IM.IntMap Int

insertRange :: (Int, Int) -> Ranges -> Ranges
insertRange (lo, hi) im
    -- Exact same lower end, insert joined range.
    | Just hi' <- mbHi =
        insertRange (lo, max hi hi') (IM.union before after)
    -- Check for overlap with before part, all keys there are lower.
    | Just ((lo', hi'), before') <- IM.maxViewWithKey before
    , hi' >= lo =
        insertRange (lo', max hi hi') (IM.union before' after)
    -- Check for overlap with after part, all keys there are higher.
    | Just ((lo', hi'), after') <- IM.minViewWithKey after
    , lo' <= hi =
        insertRange (lo, max hi hi') (IM.union before after')
    -- No overlap, finally insert.
    | otherwise = IM.insert lo hi im
  where
    (before, mbHi, after) = IM.splitLookup lo im

rangesSize :: Ranges -> Int
rangesSize = IM.foldlWithKey (\acc lo hi -> acc + (hi - lo + 1)) 0

main :: IO ()
main = pureMain $ \str -> do
    Ingredients ranges available <- NP.runParser parseIngredients str
    let part1 = length
            [n | n <- available, any (\(lo, hi) -> n >= lo && n <= hi) ranges]
        part2 = rangesSize $ foldl' (\rs r -> insertRange r rs) mempty ranges
    pure (pure part1, pure part2)
