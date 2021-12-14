module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Foldable           (foldl')
import           Data.List               (nub)
import qualified Data.Map                as M

type Polymer = [Char]
type Rules   = M.Map (Char, Char) Char

parseInput :: P.Parser Char (Polymer, Rules)
parseInput = (,)
    <$> P.many1 P.upper <* P.spaces
    <*> (foldl' insertRule M.empty <$> P.many1 parseRule)
  where
    insertRule acc (x, y, z) = M.insert (x, y) z acc
    parseRule = (,,)
        <$> P.upper <*> P.upper <* P.spaces <* P.string "->" <* P.spaces
        <*> P.upper <* P.spaces

newtype Freqs a = Freqs {unFreqs :: M.Map a Integer}

instance Ord a => Monoid (Freqs a) where
    mempty = Freqs M.empty

instance Ord a => Semigroup (Freqs a) where
    Freqs x <> Freqs y = Freqs $ M.unionWith (+) x y

single :: Ord a => a -> Freqs a
single x = Freqs (M.singleton x 1)

memoize :: Ord k => [k] -> ((k -> v) -> (k -> v)) -> (k -> v)
memoize keys f =
    let m = M.fromList [(k, f g k) | k <- keys]
        g = (M.!) m in
    g

steps :: Rules -> Polymer -> Int -> Freqs Char
steps rules polymer0 n0 =
    foldMap single polymer0 <>
    mconcat [between (x, y, n0) | (x, y) <- zip polymer0 (drop 1 polymer0)]
  where
    -- All occurring elements
    domain = (,,) <$> elements <*> elements <*> [1 .. n0]
    elements = nub $ polymer0 ++
        [c | ((x, y), z) <- M.toList rules, c <- [x, y, z]]

    -- Which elements are inserted between others?
    between = memoize domain $ \mem (x, y, n) -> case M.lookup (x, y) rules of
        Nothing | n == 1 -> mempty
        Nothing          -> mem (x, y, n - 1)
        Just z | n == 1  -> single z
        Just z           -> single z <> mem (x, z, n - 1) <> mem (z, y, n - 1)

main :: IO ()
main = pureMain $ \input -> do
    (polymer0, rules) <- P.runParser parseInput input
    let freqs10   = unFreqs $ steps rules polymer0 10
        part1     = maximum freqs10 - minimum freqs10
        freqs40   = unFreqs $ steps rules polymer0 40
        part2     = maximum freqs40 - minimum freqs40
    pure (pure part1, pure part2)
