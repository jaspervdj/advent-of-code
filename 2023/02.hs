import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.Foldable           (foldl', toList)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (All (..))

type Color      = String
type Grab       = M.Map Color Int
data GameRecord = GameRecord Int [Grab] deriving (Show)

parseGameRecords :: NP.Parser Char [GameRecord]
parseGameRecords = NP.sepBy1 gameRecord NP.newline
  where
    gameRecord = GameRecord
        <$> (NP.string "Game " *> NP.decimal <* NP.string ": ")
        <*> NP.sepBy1 grab (NP.string "; ")

    color = toList <$> NP.many1 NP.alpha
    grab  = fmap M.fromList $ NP.sepBy1
        (flip (,) <$> NP.decimal <*> (NP.spaces *> color))
        (NP.string ", ")

contains :: Ord k => M.Map k Int -> M.Map k Int -> Bool
contains bag = getAll .
    M.foldMapWithKey (\k c -> All $ c <= fromMaybe 0 (M.lookup k bag))

required :: (Ord k, Ord v) => [M.Map k v] -> M.Map k v
required = foldl' (M.unionWith max) M.empty

power :: M.Map k Int -> Int
power = M.foldl' (*) 1

main :: IO ()
main = pureMain $ \input -> do
    gameRecords <- NP.runParser parseGameRecords input
    let bag1  = M.fromList [("red", 12), ("green", 13), ("blue", 14)]
        part1 = sum [n | GameRecord n g <- gameRecords, all (contains bag1) g]
        part2 = sum [power (required g) | GameRecord _ g <- gameRecords]
    pure (pure part1, pure part2)
