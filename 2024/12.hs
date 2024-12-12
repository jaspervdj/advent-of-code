import           AdventOfCode.Dijkstra (Bfs (..), bfs)
import qualified AdventOfCode.Grid     as G
import           AdventOfCode.Main     (simpleMain)
import qualified Data.Map              as M
import qualified Data.Set              as S

regions :: Eq a => G.Grid a -> [(a, S.Set G.Pos)]
regions grid0 = case M.minViewWithKey grid0 of
    Nothing -> []
    Just ((pos, val), grid1) ->
        (val, region) :
        regions (M.filterWithKey (\k _ -> not (k `S.member` region)) grid1)
      where
        region = M.keysSet $ bfsDistances $ bfs
            (\p -> [n | n <- G.neighbours p, M.lookup n grid1 == Just val])
            (const False)
            pos

perimeter :: S.Set G.Pos -> Int
perimeter region = length
    [n | p <- S.toList region, n <- G.neighbours p, not (n `S.member` region)]

area :: S.Set G.Pos -> Int
area = S.size

sides :: S.Set G.Pos -> Int
sides region = length $ go $ S.fromList $
    (,) <$> [minBound .. maxBound] <*> S.toList region
  where
    go :: S.Set (G.Dir, G.Pos) -> [(G.Dir, S.Set G.Pos)]
    go fences0 = case S.minView fences0 of
        Nothing -> []
        Just ((d, p), fences1) | not (isFence d p) -> go fences1
        Just ((d, p), fences1) ->
            let ld      = G.turnLeft d
                rd      = G.turnRight d
                fence   = S.fromList $
                    takeWhile (isFence d) (iterate (G.move 1 ld) p) ++
                    takeWhile (isFence d) (iterate (G.move 1 rd) p)
                inFence = \(d', p') -> d' == d && S.member p' fence in
            (d, fence) :
            go (S.filter (not . inFence) fences1)

    isFence dir pos =
        (pos `S.member` region) &&
        not (G.move 1 dir pos `S.member` region)

main :: IO ()
main = simpleMain $ \input ->
    let grid0 = G.fromString input
        part1 = sum [area r * perimeter r | (_, r) <- regions grid0]
        part2 = sum [area r * sides r     | (_, r) <- regions grid0] in
    (part1, part2)
