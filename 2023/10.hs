import qualified AdventOfCode.Bfs    as Bfs
import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main
import           AdventOfCode.V2     (V2 (..))
import qualified AdventOfCode.V2.Box as Box
import qualified Data.Map            as M
import qualified Data.Set            as S

neighbours :: G.Grid Char -> G.Pos -> [G.Pos]
neighbours grid pos = case M.lookup pos grid of
    Just '|' -> move [G.U, G.D]
    Just '-' -> move [G.L, G.R]
    Just 'L' -> move [G.U, G.R]
    Just 'J' -> move [G.U, G.L]
    Just '7' -> move [G.D, G.L]
    Just 'F' -> move [G.R, G.D]
    _        -> []
  where
    move dirs = [G.move 1 dir pos | dir <- dirs]

findStart :: G.Grid Char -> (G.Pos, Char)
findStart grid
    | up    && right = (start, 'L')
    | up    && down  = (start, '|')
    | up    && left  = (start, 'J')
    | right && down  = (start, 'F')
    | right && left  = (start, '-')
    | down  && left  = (start, '7')
    | otherwise      = error "could not find start"
  where
    start = fst . head . filter ((== 'S') . snd) $ M.toList grid
    up    = M.lookup (G.move 1 G.U start) grid `elem` map Just ['|', '7', 'F']
    right = M.lookup (G.move 1 G.R start) grid `elem` map Just ['-', '7', 'J']
    down  = M.lookup (G.move 1 G.D start) grid `elem` map Just ['|', 'L', 'J']
    left  = M.lookup (G.move 1 G.L start) grid `elem` map Just ['-', 'L', 'F']

-- This is not the classical even-odd rule, we need to track a bit of extra
-- state to distiguish between the two different scenarios (x = you are here):
--
--     |       |
--     L---x---J
--
-- And:
--
--     |
--     L---x---7
--             |
--
-- To tell whether we have crossed a line after finishing the horizontal edge
-- (no in the first example, yes in the second).  This datatype stores if we
-- entered the horizontal edge from the up or down direction.
data HoriEdgeState
    = HoriEdgeDown
    | HoriEdgeUp
    | NoHoriEdge

evenodd :: G.Grid Char -> Int
evenodd grid = sum [row y minX 0 False NoHoriEdge | y <- [minY .. maxY]]
  where
    Just (Box.Box (V2 minX minY) (V2 maxX maxY)) = G.box grid

    row y x !acc inside state = case M.lookup (V2 x y) grid of
        _ | x > maxX -> acc
        Just '-' -> row y (x + 1) acc inside state
        Just '|' -> row y (x + 1) acc (not inside) NoHoriEdge
        Just '7' -> row y (x + 1) acc
            (case state of HoriEdgeDown -> not inside; _ -> inside)
            NoHoriEdge
        Just 'J' -> row y (x + 1) acc
            (case state of HoriEdgeUp -> not inside; _ -> inside)
            NoHoriEdge
        Just 'L' -> row y (x + 1) acc inside HoriEdgeDown
        Just 'F' -> row y (x + 1) acc inside HoriEdgeUp
        _ -> row y (x + 1) (if inside then acc + 1 else acc) inside state

main :: IO ()
main = simpleMain $ \str ->
    let gridS                = G.fromString str
        (startPos, startTok) = findStart gridS
        grid                 = M.insert startPos startTok gridS

        distances = Bfs.distances $ Bfs.bfs Bfs.defaultOptions
            { Bfs.neighbours = neighbours grid
            , Bfs.start      = S.singleton startPos
            }

        part1 = maximum distances

        relevant = M.filterWithKey (\k _ -> k `M.member` distances) grid
        part2    = evenodd relevant in

    (part1, part2)
