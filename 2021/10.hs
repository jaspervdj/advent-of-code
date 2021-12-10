{-# LANGUAGE LambdaCase #-}
module Main where

import           AdventOfCode.Main  (simpleMain)
import           Data.Foldable      (foldl')
import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (mapMaybe)

data Parens a = Par [a] [a] | Bad (NonEmpty a) deriving (Show)

mkParens :: Char -> Parens Char
mkParens c
    | c `elem` ">])}" = Par [c] []
    | c == '<'        = Par [] ['>']
    | c == '['        = Par [] [']']
    | c == '('        = Par [] [')']
    | c == '{'        = Par [] ['}']
    | otherwise       = Bad (c :| [])

instance Eq a => Semigroup (Parens a) where
    Bad x          <> Bad y                   = Bad (x <> y)
    Bad x          <> _                       = Bad x
    _              <> Bad y                   = Bad y
    Par _ (x : _)  <> Par (y : _)  _ | x /= y = Bad (y :| [])
    Par l (_ : xs) <> Par (_ : ys) r          = Par l xs <> Par ys r
    Par l []       <> Par ys       r          = Par (l ++ ys) r
    Par l xs       <> Par []       r          = Par l (r ++ xs)

instance Eq a => Monoid (Parens a) where mempty = Par [] []

firstIllegal :: Parens a -> Maybe a
firstIllegal = \case
    Par (c : _) _ -> Just c
    Bad (c :| _)  -> Just c
    _             -> Nothing

illegalPoints :: Char -> Int
illegalPoints = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> 0

completeEnd :: Parens a -> [a]
completeEnd = \case
    Par _ r -> r
    _       -> []

incompletePoints :: String -> Int
incompletePoints = foldl' (\acc c -> acc * 5 + points c) 0
  where
    points = \case
        ')' -> 1
        ']' -> 2
        '}' -> 3
        '>' -> 4
        _   -> 0

main :: IO ()
main = simpleMain $ \input ->
    let parens      = map (foldMap mkParens) $ lines input
        part1       = sum . map illegalPoints $ mapMaybe firstIllegal parens
        completions = filter (not . null) $ map completeEnd parens
        scores      = map incompletePoints completions
        part2       = sort scores !! (length completions `div` 2) in
    (part1, part2)
