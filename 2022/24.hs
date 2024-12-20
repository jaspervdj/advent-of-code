import qualified AdventOfCode.Bfs          as Bfs
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           (V2 (..), (.+.))
import           Control.Monad             (guard)
import qualified Data.Set                  as Set
import qualified Data.Vector               as V

data Tile = Wall | Floor | Blizzard G.Dir

parseTile :: Char -> Either String Tile
parseTile c = case c of
    '#' -> Right Wall
    '.' -> Right Floor
    '^' -> Right $ Blizzard G.U
    '>' -> Right $ Blizzard G.R
    'v' -> Right $ Blizzard G.D
    '<' -> Right $ Blizzard G.L
    _   -> Left $ "Unknown tile: " ++ show c

precomputeBlizzards :: G.Grid Tile -> V.Vector (G.Grid Bool)
precomputeBlizzards grid0 =
    V.map toGrid $ V.iterateN period step blizzards0
  where
    period     = lcm (G.gridWidth grid0) (G.gridHeight grid0)
    blizzards0 = [(d, pos) | (pos, Blizzard d) <- G.toList grid0]

    step blizzards = [(b, wrap (G.move 1 b p)) | (b, p) <- blizzards]

    toGrid blizzards =
        let set = Set.fromList $ map snd blizzards in
        G.generate (G.gridWidth grid0) (G.gridHeight grid0) $ \pos ->
            Set.member pos set

    wrap (V2 x y) = V2 (x `mod` G.gridWidth grid0) (y `mod` G.gridHeight grid0)

removeEdges :: G.Grid a -> G.Grid a
removeEdges big = G.generate (G.gridWidth big - 2) (G.gridHeight big - 2) $
    \pos -> G.index (pos .+. V2 1 1) big

main :: IO ()
main = pureMain $ \str -> do
    grid <- G.fromString str >>= traverse parseTile >>= pure . removeEdges
    let blizzards = precomputeBlizzards grid
        period    = V.length blizzards

        dest  = V2 (G.gridWidth grid - 1) (G.gridHeight grid)
        start = V2 0 (-1)

        neighbourStates (time, pos) = do
            pos' <- pos : map (\d -> G.move 1 d pos) [G.U, G.R, G.D, G.L]
            let time' = (time + 1) `mod` period
            guard $ case G.lookup pos' $ blizzards V.! time' of
                Nothing       -> pos' == start || pos' == dest
                Just blizzard -> not blizzard
            pure (time', pos')

        part1 = maybe 0 snd $ Bfs.goal $ Bfs.bfs Bfs.defaultOptions
            { Bfs.neighbours = neighbourStates
            , Bfs.find       = (== dest) . snd
            , Bfs.start      = Set.singleton (0, start)
            }

        part2a = maybe 0 snd $ Bfs.goal $ Bfs.bfs Bfs.defaultOptions
            { Bfs.neighbours = neighbourStates
            , Bfs.find       = (== start) . snd
            , Bfs.start      = Set.singleton (part1, dest)
            }

        part2b = maybe 0 snd $ Bfs.goal $ Bfs.bfs Bfs.defaultOptions
            { Bfs.neighbours = neighbourStates
            , Bfs.find       = (== dest) . snd
            , Bfs.start      = Set.singleton (part1 + part2a, start)
            }

        part2 = part1 + part2a + part2b

    pure (pure part1, pure part2)
