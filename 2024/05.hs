import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import           Data.Foldable           (toList)
import           Data.List               (sortOn)
import qualified Data.Map                as M
import           Data.Maybe              (isNothing, listToMaybe, mapMaybe)

data Constraint = Before Int Int deriving (Show)
type Update = [Int]
data Input = Input [Constraint] [Update] deriving (Show)

parseInput :: NP.Parser Char Input
parseInput = Input <$> many constr <*> many update
  where
    constr = Before <$> NP.decimal <* NP.char '|' <*> NP.decimal <* NP.spaces
    update = toList <$> NP.sepBy1 NP.decimal (NP.char ',') <* NP.spaces

middlePage :: Update -> Int
middlePage u = u !! (length u `div` 2)

newtype Index = Index Int deriving (Eq, Ord)
type Indexed = M.Map Int Index

toIndexed :: Update -> Indexed
toIndexed update = M.fromList $ zip update $ map Index [0 ..]

fromIndexed :: Indexed -> Update
fromIndexed = map fst . sortOn snd . M.toList

-- | Checks and fixes a constraint by swapping.
fixConstraint :: Constraint -> Indexed -> Maybe Indexed
fixConstraint (Before x y) idxs = do
    i <- M.lookup x idxs
    j <- M.lookup y idxs
    if i < j then Nothing else Just $ M.insert x j $ M.insert y i idxs

-- | Checks validity of all constraints.
checkUpdate :: [Constraint] -> Update -> Bool
checkUpdate constraints update =
    all (\c -> isNothing $ fixConstraint c idxs) constraints
  where
    idxs = toIndexed update

-- | Repeatedly fixes constraints until all is valid.
fixUpdate :: [Constraint] -> Update -> Maybe Update
fixUpdate constraints update = fromIndexed . go <$> step (toIndexed update)
  where
    step idxs = listToMaybe $ mapMaybe (\c -> fixConstraint c idxs) constraints
    go idxs   = maybe idxs go (step idxs)

main :: IO ()
main = pureMain $ \input -> do
    Input constraints updates <- NP.runParser parseInput input
    let part1 = sum $ map middlePage $ filter (checkUpdate constraints) updates
        part2 = sum $ map middlePage $ mapMaybe (fixUpdate constraints) updates
    pure (pure part1, pure part2)
