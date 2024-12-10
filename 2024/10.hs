import           AdventOfCode.Dijkstra     (Bfs (..), bfs)
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           Control.Monad             (guard)
import           Data.Char                 (digitToInt, isDigit)
import qualified Data.Map                  as M
import           Data.Maybe                (mapMaybe)
import           Data.Traversable          (for)

trailheads :: G.Grid Int -> [G.Pos]
trailheads = map fst . filter ((== 0) . snd) . G.toList

score :: G.Grid Int -> G.Pos -> Int
score grid trailhead = length $ filter (\p -> grid G.! p == 9) $ M.keys $
    bfsDistances $ bfs
        (\p -> do
            n <- filter (`G.member` grid) $ G.neighbours p
            guard $ grid G.! n == grid G.! p + 1
            pure n)
        (const False)
        trailhead

ratings :: G.Grid Int -> Int
ratings grid = go 0 $ M.fromList $ zip (trailheads grid) $ repeat 1
  where
    go h level | h >= 9 = sum level
    go h level          = go (h + 1) $ M.fromList $ do
       p <- map fst $ filter ((== h + 1) . snd) $ G.toList grid
       pure (p, sum $ mapMaybe (`M.lookup` level) $ G.neighbours p)

main :: IO ()
main = pureMain $ \input -> do
    grid0 <- G.fromString input
    grid1 <- for grid0 $ \c ->
        if isDigit c then pure (digitToInt c) else Left $ "bad char: " ++ [c]
    let part1      = sum $ map (score grid1) (trailheads grid1)
        part2      = ratings grid1
    pure (pure part1, pure part2)
