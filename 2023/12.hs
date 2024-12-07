import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Foldable           (toList)
import           Data.List               (intercalate, tails)
import qualified Data.Map                as M

data Tile = Operational | Damaged | Unknown deriving (Eq, Ord, Show)
data Row = Row [Tile] [Int] deriving (Eq, Ord, Show)

parseInput :: NP.Parser Char [Row]
parseInput = fmap toList $ NP.many1 $ Row
    <$> (toList <$> NP.many1 mbTile <* NP.spaces)
    <*> (NP.sepBy NP.decimal (NP.char ',') <* NP.spaces)
  where
    mbTile =
        (Operational <$ NP.char '.') <|>
        (Damaged     <$ NP.char '#') <|>
        (Unknown     <$ NP.char '?')

-- Refactored to take the recursive call as an argument.
solve :: (Row -> Int) -> Row -> Int
solve _ (Row [] (_ : _)) = 0
solve _ (Row tiles [])
    | any (== Damaged) tiles = 0
    | otherwise              = 1
solve rec (Row (Operational : tiles) record) = rec (Row tiles record)
solve rec (Row (Damaged : tiles) (n : record))
    | length dmgd /= n - 1 || any (== Operational) dmgd = 0
    | not (null post) && head post == Damaged = 0
    | otherwise = rec (Row (drop 1 post) record)
  where
    (dmgd, post) = splitAt (n - 1) tiles
solve rec (Row (Unknown : tiles) record) =
    rec (Row (Operational : tiles) record) +
    rec (Row (Damaged : tiles) record)

-- Removes the recursive call again to restore part 1.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Constructs a memoizing recursive call for subrows.
memoize :: ((Row -> a) -> (Row -> a)) -> Row -> a
memoize f row@(Row tiles record) = cache M.! row
  where
    keys  = Row <$> tails tiles <*> tails record
    cache = M.fromList [(k, f rec k) | k <- keys]
    rec k = case M.lookup k cache of
        Nothing -> f rec k
        Just x  -> x

unfold :: Row -> Row
unfold (Row tiles record) = Row
    (intercalate [Unknown] $ replicate 5 tiles)
    (concat $ replicate 5 record)

main :: IO ()
main = pureMain $ \instr -> do
    rows <- NP.runParser parseInput instr
    let part1 = sum $ map (fix solve) rows
        part2 = sum $ map (memoize solve . unfold) rows
    pure (pure part1, pure part2)
