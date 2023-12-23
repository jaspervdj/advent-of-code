import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..))
import           Control.Monad             (guard)
import           Control.Monad.Except      (throwError)
import qualified Data.Map                  as M
import           Data.Maybe                (listToMaybe)
import           Data.Semigroup            (Max (..))
import qualified Data.Set                  as S

data Tile1 = Tile2 Tile2 | Slope G.Dir deriving (Eq, Show)
data Tile2 = Forest | Path deriving (Eq, Show)

parseTile :: Char -> Either String Tile1
parseTile c = case c of
    '.' -> pure $ Tile2 Path
    '#' -> pure $ Tile2 Forest
    '^' -> pure $ Slope G.U
    '>' -> pure $ Slope G.R
    'v' -> pure $ Slope G.D
    '<' -> pure $ Slope G.L
    _   -> throwError $ "unknown tile: " ++ show c

firstPathOnRow :: G.Grid Tile1 -> Int -> Maybe G.Pos
firstPathOnRow grid y = listToMaybe $ do
    pos <- (\x -> (V2 x y)) <$> [0 .. G.gridWidth grid - 1]
    guard $ G.lookup pos grid == Just (Tile2 Path)
    pure pos

longestHike1 :: G.Grid Tile1 -> G.Pos -> G.Pos -> Maybe (Max Int)
longestHike1 grid start end = go S.empty start
  where
    go :: S.Set G.Pos -> G.Pos -> Maybe (Max Int)
    go visited pos = case G.lookup pos grid of
        _ | pos `S.member` visited -> Nothing
        _ | pos == end -> Just (Max $ S.size visited)
        Nothing -> Nothing
        Just (Tile2 Forest) -> Nothing
        Just (Slope dir) ->
            let next = G.move 1 dir pos in
            go (S.insert pos visited) next
        Just (Tile2 Path) -> mconcat $ do
            next <- G.neighbours pos
            pure $ go (S.insert pos visited) next

type Graph = M.Map G.Pos (M.Map G.Pos Int)

-- | Converts the grid to a graph naively.
gridToGraph :: G.Grid Tile2 -> Graph
gridToGraph grid = M.fromList $ do
    (pos, Path) <- G.toList grid
    let nbs = [(n, 1) | n <- G.neighbours pos, G.lookup n grid == Just Path]
    pure (pos, M.fromList nbs)

-- | Finds an edge that can be removed, and removes it.
compressGraphOnce :: Graph -> Maybe Graph
compressGraphOnce graph = do
    (k, (p, d1), (q, d2)) <- listToMaybe $ do
        (k, [n1, n2]) <- M.toList $ M.toList <$> graph
        pure (k, n1, n2)
    pure .
        M.adjust (M.insert q (d1 + d2) . M.delete k) p .
        M.adjust (M.insert p (d1 + d2) . M.delete k) q .
        M.delete k $ graph

-- | Removes as many edges from the graph as possible.
compressGraph :: Graph -> Graph
compressGraph g = maybe g compressGraph $ compressGraphOnce g

longestHike2 :: Graph -> G.Pos -> G.Pos -> Maybe (Max Int)
longestHike2 graph start end = go 0 S.empty start
  where
    go :: Max Int -> S.Set G.Pos -> G.Pos -> Maybe (Max Int)
    go dist visited pos = case M.lookup pos graph of
        _ | pos `S.member` visited -> Nothing
        _ | pos == end -> Just dist
        Nothing -> Nothing
        Just nbs -> mconcat $ do
            (n, d) <- M.toList nbs
            pure $ go (dist + Max d) (S.insert pos visited) n

main :: IO ()
main = pureMain $ \str -> do
    grid <- G.fromString str >>= traverse parseTile
    let (_, h) = (G.gridWidth grid, G.gridHeight grid)
    start <- maybe (throwError "no start") pure $ firstPathOnRow grid 0
    end <- maybe (throwError "no end") pure $ firstPathOnRow grid (h - 1)
    let part1 = maybe (throwError "no longest hike") (pure . getMax) $
            longestHike1 grid start end
        graph = compressGraph $ gridToGraph $
            (\t1 -> case t1 of Slope _ -> Path; Tile2 t2 -> t2) <$> grid
        part2 = maybe (throwError "no longest hike") (pure . getMax) $
            longestHike2 graph start end
    pure (part1, part2)
