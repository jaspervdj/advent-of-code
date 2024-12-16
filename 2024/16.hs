import qualified AdventOfCode.Dijkstra     as Dijkstra
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import qualified Data.Map                  as M
import qualified Data.Set                  as S

data Tile = Start | End | Wall | Empty deriving (Eq, Show)
data Reindeer = Reindeer {rPos :: G.Pos, rDir :: G.Dir} deriving (Eq, Ord, Show)

parseTile :: Char -> Either String Tile
parseTile c = case c of
    'S' -> pure Start
    'E' -> pure End
    '#' -> pure Wall
    '.' -> pure Empty
    _   -> Left $ "Unknown char: " ++ show c

neighbours :: G.Grid Tile -> Reindeer -> [(Int, Reindeer)]
neighbours grid (Reindeer p d) =
    [(1, Reindeer q d) | q `G.member` grid, grid G.! q /= Wall] ++
    [(1000, Reindeer p (G.turnLeft d))] ++
    [(1000, Reindeer p (G.turnRight d))]
  where
    q = G.move 1 d p

retrace :: G.Pos -> M.Map Reindeer (Int, S.Set Reindeer) -> S.Set G.Pos
retrace goal prevs =
    foldMap (go prevs) [Reindeer goal d | d <- [minBound .. maxBound]]
  where
    go prevs0 r = S.singleton (rPos r) <> case M.lookup r prevs0 of
        Nothing      -> S.empty
        Just (_, ps) -> foldMap (go $ M.delete r prevs0) ps

main :: IO ()
main = pureMain $ \str -> do
    grid <- G.fromString str >>= traverse parseTile
    start <- case filter ((== Start) . snd) $ G.toList grid of
        [(p, _)] -> pure p
        _        -> Left $ "Unclear start"
    end <- case filter ((== End) . snd) $ G.toList grid of
        [(p, _)] -> pure p
        _        -> Left $ "Unclear end"
    let goal (Reindeer p _) = p == end

        search1 = Dijkstra.dijkstra Dijkstra.Options
            { Dijkstra.neighbours = neighbours grid
            , Dijkstra.find       = Dijkstra.FindOne goal
            , Dijkstra.start      = S.singleton (Reindeer start G.R)
            }
        part1 = maybe (Left "no solution") (pure . fst) $ Dijkstra.goal search1

        search2 = Dijkstra.dijkstra Dijkstra.Options
            { Dijkstra.neighbours = neighbours grid
            , Dijkstra.find       = Dijkstra.FindAll goal
            , Dijkstra.start      = S.singleton (Reindeer start G.R)
            }
        part2 = S.size $ retrace end $ Dijkstra.back search2

    pure (part1, pure part2)
