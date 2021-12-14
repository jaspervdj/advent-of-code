{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Foldable           (foldl')
import qualified Data.Map                as M
import           Data.Monoid             (Endo (..))
import           Data.Semigroup          (stimes)

-- Preliminary pair type, and a type for rules.
data Pair a  = Pair a a deriving (Eq, Foldable, Functor, Ord, Show)
type Rules a = M.Map (Pair a) a

-- Count frequencies for some type
newtype Freqs a = Freqs {unFreqs :: M.Map a Integer} deriving (Show)

-- Just a single item is a frequency of 1
single :: Ord a => a -> Freqs a
single x = Freqs (M.singleton x 1)

-- We can add frequencies together
instance Ord a => Semigroup (Freqs a) where
    Freqs x <> Freqs y = Freqs $ M.unionWith (+) x y

-- Zero frequencies
instance Ord a => Monoid (Freqs a) where mempty = Freqs M.empty

-- We can multiply frequencies
times :: Integer -> Freqs a -> Freqs a
times k (Freqs m) = Freqs ((* k) <$> m)

-- If we have frequencies of collections, we can take the frequencies of the
-- individual items.
separate :: (Foldable f, Ord a) => Freqs (f a) -> Freqs a
separate = M.foldMapWithKey (\f n -> foldMap (times n . single) f) . unFreqs

-- You can construct frequencies of pairs from the adjacent elements of a list
initial :: Ord a => [a] -> Freqs (Pair a)
initial l = mconcat $ zipWith (\x y -> single $ Pair x y) l (drop 1 l)

-- A collection of rules is a function of frequencies of pairs to new
-- frequencies of pairs
step :: Ord a => Rules a -> Freqs (Pair a) -> Freqs (Pair a)
step rules (Freqs m) =
    M.foldMapWithKey (\pair@(Pair x y) n -> case M.lookup pair rules of
        -- Keep the existing pairs
        Nothing -> times n (single pair)
        -- Introduce new pairs if there is a rule match.
        Just z  ->
            times n (single (Pair x z)) <> times n (single (Pair z y))) m

-- Because we want to demonstrate the slow and fast solution, lets use a type
-- that can step N times.
type Solver a = Int -> Freqs (Pair a) -> Freqs (Pair a)

-- We can now write a solver for the puzzle.  We leave out the part that steps
-- N times.
solve :: Ord a => Solver a -> [a] -> Int -> Integer
solve solver polymer steps =
    let freqs = fmap (`div` 2) . unFreqs $
            single (head polymer) <> single (last polymer) <>
            separate (solver steps (initial polymer)) in
    maximum freqs - minimum freqs

-- A slow solver just applies 'step' N times.
slowSolver :: Ord a => Rules a -> Solver a
slowSolver rules n freqs = iterate (step rules) freqs !! n

-- A fast solver uses the semigroup instance of `Endo Frequency`
fastSolver :: Ord a => Rules a -> Solver a
fastSolver rules n = appEndo . stimes n . Endo $ step rules

-- Input parser
parseInput :: P.Parser Char (String, Rules Char)
parseInput = (,)
    <$> P.many1 P.upper <* P.spaces
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
    let part1 = solve (slowSolver rules) polymer 10
        part2 = solve (fastSolver rules) polymer 40
    pure (pure part1, pure part2)
