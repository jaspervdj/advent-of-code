module Main where

import           Data.List (nub)
import Data.Char (isSpace)

class Increment a where
    increment :: a -> a

newtype Lower = Lower {unLower :: Char} deriving (Eq, Ord, Show)

instance Bounded Lower where
    minBound = Lower 'a'
    maxBound = Lower 'z'

instance Increment Lower where
    increment l@(Lower x)
        | l == maxBound = minBound
        | otherwise     = Lower (succ x)

instance (Bounded a, Eq a, Increment a) => Increment [a] where
    increment =
        \xs -> let (ys, carry) = go xs in if carry then minBound : ys else ys
      where
        go [] = ([], True)
        go (x : xs) =
            let (ys, carry) = go xs
                y           = increment x in
            if carry then (y : ys, x == maxBound) else (x : ys, False)

hasIncreasingStraight :: (Bounded a, Eq a, Increment a) => [a] -> Bool
hasIncreasingStraight (x : y : z : zs) =
    (increment x == y && increment y == z && x /= maxBound && y /= maxBound) ||
    hasIncreasingStraight (y : z : zs)
hasIncreasingStraight _ = False

pairs :: Eq a => [a] -> [a]
pairs (x : y : zs)
    | x == y    = x : pairs zs
    | otherwise = pairs (y : zs)
pairs _         = []

twoDifferentPairs :: Eq a => [a] -> Bool
twoDifferentPairs = (>= 2) . length . nub . pairs

validPassword :: [Lower] -> Bool
validPassword password =
    hasIncreasingStraight password &&
    not (any forbidden password) &&
    twoDifferentPairs password
  where
    forbidden (Lower c) = c == 'i' || c == 'o' || c == 'l'

main :: IO ()
main = do
    pw0 <- map Lower . filter (not . isSpace) <$> getContents
    let pw1 = head $ filter validPassword $ iterate increment pw0
        pw2 = head $ filter validPassword $ iterate increment $ increment pw1
    putStrLn $ map unLower pw1
    putStrLn $ map unLower pw2
