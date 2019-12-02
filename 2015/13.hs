module Main where

import           Data.List   (maximum, nub, permutations)
import qualified Data.Map    as Map
import           Data.Maybe  (fromMaybe)
import           Data.Monoid (Sum (..))
import           Data.Tuple  (swap)
import qualified System.IO   as IO
import           Text.Read   (readMaybe)

type PairTable a s = Map.Map (a, a) s

readHappinessTable :: IO.Handle -> IO (PairTable String Int)
readHappinessTable h = do
    fmap Map.unions $ IO.hGetContents h >>= mapM readHappiness . lines
  where
    readHappiness line = case words (filter (/= '.') line) of
        [x, _, "gain", s, _, _, _, _, _, _, y] | Just n <- readMaybe s ->
            pure $ Map.singleton (x, y) n
        [x, _, "lose", s, _, _, _, _, _, _, y] | Just n <- readMaybe s ->
            pure $ Map.singleton (x, y) (negate n)
        _ -> fail $ "Couldn't parse: " ++ show line

score :: (Ord a, Monoid s) => PairTable a s -> [a] -> s
score table attendees =
    mconcat .
    map (\xy -> fromMaybe mempty (Map.lookup xy table)) .
    concatMap (\xy -> [xy, swap xy]) $
    zip (drop 1 attendees ++ take 1 attendees) attendees

permutations' :: [a] -> [[a]]
permutations' []       = [[]]
permutations' (x : xs) = map (x :) (permutations xs)

main :: IO ()
main = do
    table <- fmap (fmap Sum) $ readHappinessTable IO.stdin
    let attendees = nub $ map (fst . fst) $ Map.toList table
    print $ getSum $ maximum $ map (score table) $ permutations' attendees
    let attendees' = "Jasper" : attendees
    print $ getSum $ maximum $ map (score table) $ permutations' attendees'
