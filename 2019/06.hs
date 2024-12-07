{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Bifunctor          (second)
import           Data.Foldable           (toList)
import qualified Data.Map                as Map
import           Data.Semigroup          (Min (..))
import qualified Data.Set                as Set
import qualified System.IO               as IO

type Orbit a = (a, a)

parseOrbits :: NP.Parser Char [Orbit String]
parseOrbits = fmap toList $ NP.many1 $ (,)
    <$> (toList <$> NP.many1 (NP.alpha <|> NP.digit) <* NP.char ')')
    <*> (toList <$> NP.many1 (NP.alpha <|> NP.digit) <* NP.spaces)

data OrbitMap a = OrbitMap
    { omEdges :: Map.Map a (Set.Set a)
    , omRoots :: Set.Set a
    } deriving (Show)

makeOrbitMap :: Ord a => [Orbit a] -> OrbitMap a
makeOrbitMap orbits =
    OrbitMap {..}
  where
    omEdges = Map.fromListWith Set.union $ second Set.singleton <$> orbits
    omRoots = Map.foldl'
        (\roots set -> roots `Set.difference` set) (Map.keysSet omEdges) omEdges

data OrbitTree a = OrbitTree a [OrbitTree a] deriving (Foldable, Show)

makeOrbitForest :: Ord a => OrbitMap a -> [OrbitTree a]
makeOrbitForest OrbitMap {..} =
    map makeOrbitTree (Set.toList omRoots)
  where
    makeOrbitTree k =
        OrbitTree k $ map makeOrbitTree $
        maybe [] Set.toList (Map.lookup k omEdges)

orbitCounts :: OrbitTree a -> OrbitTree Int
orbitCounts = go 0
  where
    go !n (OrbitTree _ children) = OrbitTree n (map (go (n + 1)) children)

data OrbitPath
    = Found (Min Int)
    | Search (Maybe (Min Int)) (Maybe (Min Int))
    deriving (Show)

instance Semigroup OrbitPath where
    Found x      <> Found y      = Found (x <> y)
    Found x      <> _            = Found x
    _            <> Found y      = Found y
    Search l1 r1 <> Search l2 r2 = case (l1 <> l2, r1 <> r2) of
        (Just x, Just y) -> Found (x + y)
        (l, r)           -> Search l r

instance Monoid OrbitPath where
    mempty = Search Nothing Nothing

deeper :: OrbitPath -> OrbitPath
deeper (Found x)    = Found x
deeper (Search l r) = Search (fmap succ l) (fmap succ r)

orbitPath :: Eq a => a -> a -> OrbitTree a -> OrbitPath
orbitPath from to (OrbitTree x children) = mconcat $
    [Search (Just 0) Nothing | x == from] ++
    [Search Nothing (Just 0) | x == to  ] ++
    map (deeper . orbitPath from to) children

main :: IO ()
main = do
    f <- makeOrbitForest . makeOrbitMap <$> NP.hRunParser IO.stdin parseOrbits
    print $ sum $ map (sum . orbitCounts) f
    case foldMap (orbitPath "SAN" "YOU") f of
        Found (Min n) -> print (n - 2)  -- Don't count initial two orbits.
        _             -> putStrLn "No path found"
