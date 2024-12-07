import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Foldable           (toList)
import qualified Data.Map                as M
import           Data.Monoid             (Sum (..))

type Fish = Int

stepFish :: Fish -> [Fish]
stepFish n = if n <= 0 then [6, 8] else [n - 1]

-- | This is an optimization for part2.  We could also use it for part1, but I
-- think it's interesting how we can derive this second part while we keep
-- using the `stepFish` from part1.
newtype Population = Pop {unPop :: M.Map Fish Integer} deriving (Show)
instance Monoid Population where mempty = Pop mempty
instance Semigroup Population where Pop l <> Pop r = Pop $ M.unionWith (+) l r

size :: Population -> Integer
size = getSum . foldMap Sum . unPop

singleton :: Fish -> Population
singleton fish = Pop $ M.singleton fish 1

times :: Integer -> Population -> Population
times n (Pop pop) = Pop $ (n *) <$> pop

stepPopulation :: Population -> Population
stepPopulation = M.foldMapWithKey
    (\fish freq -> times freq . foldMap singleton $ stepFish fish) . unPop

main :: IO ()
main = pureMain $ \input -> do
    fishes <- toList <$> P.runParser (P.decimal `P.sepBy1` P.char ',') input
    let part1 = length $ iterate (concatMap stepFish) fishes !! 80
        part2 = size $ iterate stepPopulation (foldMap singleton fishes) !! 256
    pure (pure part1, pure part2)
