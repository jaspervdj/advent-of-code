{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Foldable           (foldl', toList)
import qualified Data.Map                as M
import           Data.Monoid             (Endo (..))
import           Data.Semigroup          (stimes)

-- Preliminary pair type, and a type for rules.
data Pair a  = Pair a a deriving (Eq, Foldable, Functor, Ord, Show)
type Rules a = M.Map (Pair a) a

-- Count frequencies for some type
newtype Freqs a = Freqs {unFreqs :: M.Map a Integer} deriving (Show)

-- A single item, n times.
times :: Integer -> a -> Freqs a
times k x = Freqs (M.singleton x k)

-- We can add frequencies together
instance Ord a => Semigroup (Freqs a) where
    Freqs x <> Freqs y = Freqs $ M.unionWith (+) x y

-- Zero frequencies
instance Ord a => Monoid (Freqs a) where mempty = Freqs M.empty

-- If we have frequencies of collections, we can take the frequencies of the
-- individual items.
separate :: (Foldable f, Ord a) => Freqs (f a) -> Freqs a
separate = M.foldMapWithKey (\f n -> foldMap (times n) f) . unFreqs

-- You can construct frequencies of pairs from the adjacent elements of a list
initial :: Ord a => [a] -> Freqs (Pair a)
initial l = mconcat $ zipWith (\x y -> times 1 (Pair x y)) l (drop 1 l)

-- A collection of rules is a function of frequencies of pairs to new
-- frequencies of pairs
step :: Ord a => Rules a -> Freqs (Pair a) -> Freqs (Pair a)
step rules (Freqs m) =
    M.foldMapWithKey (\pair@(Pair x y) n -> case M.lookup pair rules of
        -- Keep the existing pairs
        Nothing -> times n pair
        -- Introduce new pairs if there is a rule match.
        Just z  -> times n (Pair x z) <> times n (Pair z y)) m

-- We can now write a solver for the puzzle.
solve :: Ord a => Rules a -> [a] -> Int -> Integer
solve rules polymer n =
    let steps = appEndo . stimes n . Endo $ step rules
        freqs = fmap (`div` 2) . unFreqs $
            times 1 (head polymer) <> times 1 (last polymer) <>
            separate (steps $ initial polymer) in
    maximum freqs - minimum freqs

-- Input parser
parseInput :: P.Parser Char (String, Rules Char)
parseInput = (,)
    <$> (toList <$> P.many1 P.upper) <* P.spaces
    <*> (foldl' insertRule M.empty <$> P.many1 parseRule)
  where
    insertRule acc (x, y, z) = M.insert (Pair x y) z acc
    parseRule = (,,)
        <$> P.upper <*> P.upper <* P.spaces <* P.string "->" <* P.spaces
        <*> P.upper <* P.spaces

-- Pur everything together
main :: IO ()
main = pureMain $ \input -> do
    (polymer, rules) <- P.runParser parseInput input
    let part1 = solve rules polymer 10
        part2 = solve rules polymer 40
    pure (pure part1, pure part2)
