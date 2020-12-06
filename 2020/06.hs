module Main where

import           Data.Function (on)
import           Data.List     (foldl', groupBy)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import qualified System.IO     as IO

type Forms a = [Set.Set a]

readForms :: IO.Handle -> IO [Forms Char]
readForms h =
    map (map Set.fromList) . filter (/= [""]) .
    groupBy (on (&&) (not . null)) . lines <$> IO.hGetContents h

intersections :: Ord a => [Set.Set a] -> Set a
intersections []       = Set.empty
intersections (x : xs) = foldl' Set.intersection x xs

main :: IO ()
main = do
    forms <- readForms IO.stdin
    print . sum $ map (Set.size . Set.unions) forms
    print . sum $ map (Set.size . intersections) forms
