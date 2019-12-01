{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List  as L
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as Set
import qualified System.IO  as IO
import           Text.Read  (readMaybe)

type Distances = Map.Map (String, String) Int

readDistances :: IO.Handle -> IO Distances
readDistances h =
    fmap Map.unions (IO.hGetContents h >>= mapM readDistance . lines)
  where
    readDistance line = case words line of
        [x, "to", y, "=", d] | Just n <- readMaybe d -> pure $
            Map.fromList [((x, y), n), ((y, x), n)]
        _ -> fail $ "Couldn't parse line: " ++ show line

distance :: Distances -> [String] -> Int
distance distances = go 0
  where
    go !acc (x : y : zs) =
        go (acc + fromMaybe 0 (Map.lookup (x, y) distances)) (y : zs)
    go !acc _ = acc

main :: IO ()
main = do
    distances <- readDistances IO.stdin
    let cities = Set.toList . Set.fromList . map fst $ Map.keys distances
    print $ minimum $ map (distance distances) (L.permutations cities)
    print $ maximum $ map (distance distances) (L.permutations cities)
