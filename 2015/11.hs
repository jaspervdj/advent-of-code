{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Data.Char       (isSpace)
import           Data.List       (nub)
import           Data.List.Extra (lexicographicSuccessor)

newtype Lower = Lower {unLower :: Char} deriving (Enum, Eq, Ord, Show)

instance Bounded Lower where
    minBound = Lower 'a'
    maxBound = Lower 'z'

hasIncreasingStraight :: (Bounded a, Eq a, Enum a) => [a] -> Bool
hasIncreasingStraight (x : y : z : zs) =
    (x /= maxBound && y /= maxBound && succ x == y && succ y == z) ||
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
    let pw1 = head $ filter validPassword $ iterate lexicographicSuccessor pw0
        pw2 = head $ filter validPassword $ iterate lexicographicSuccessor $
            lexicographicSuccessor pw1
    putStrLn $ map unLower pw1
    putStrLn $ map unLower pw2
