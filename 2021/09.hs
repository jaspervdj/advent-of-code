{-# LANGUAGE LambdaCase #-}
import qualified AdventOfCode.Bfs  as Bfs
import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main (pureMain)
import           Control.Monad     (guard)
import           Data.Char         (digitToInt, isDigit)
import           Data.List         (sortOn)
import qualified Data.Map          as M
import           Data.Maybe        (maybeToList)
import           Data.Ord          (Down (..))
import qualified Data.Set          as S
import           Data.Traversable  (for)

-- Find low points
lowPoints :: Ord a => G.Grid a -> [(G.Pos, a)]
lowPoints grid = do
    (pos, height) <- M.toList grid
    guard . and $ do
        n <- G.neighbours pos
        h <- maybeToList $ M.lookup n grid
        pure $ h > height
    pure (pos, height)

-- Extend basin starting from low point
basin :: Ord a => G.Grid a -> G.Pos -> [G.Pos]
basin grid start = Bfs.reachable $ Bfs.bfs Bfs.defaultOptions
    { Bfs.neighbours = neighbours
    , Bfs.start      = S.singleton start
    }
  where
    neighbours :: G.Pos -> [G.Pos]
    neighbours pos = do
        height <- maybeToList $ M.lookup pos grid
        n <- G.neighbours pos
        h <- maybeToList $ M.lookup n grid
        guard $ h > height
        pure n

main :: IO ()
main = pureMain $ \input -> do
    grid <- for (G.fromString input) $ \case
        c | isDigit c -> Right $ digitToInt c
        c             -> Left $ "Unknown char " ++ show c

    let lps    = lowPoints grid
        part1  = sum $ map (succ . snd) lps
        grid'  = M.filter (< 9) grid
        basins = map (basin grid' . fst) lps
        part2  = product . take 3 $ sortOn Down (map length basins)

    pure (pure part1, pure part2)
