import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           ((.+.), (.-.))
import qualified Data.Map                  as M
import qualified Data.Set                  as S

parseAntennas :: G.Grid Char -> M.Map Char [G.Pos]
parseAntennas grid = M.unionsWith (<>)
    [M.singleton c [p] | (p, c) <- G.toList grid, c /= '.']

pairs :: Ord a => [a] -> [(a, a)]
pairs l = [(x, y) | x <- l, y <- l, x < y]

antinodes1 :: G.Grid a -> G.Pos -> G.Pos -> [G.Pos]
antinodes1 grid p q =
    let d = q .-. p in filter (`G.member` grid) [p .-. d, q .+. d]

antinodes2 :: G.Grid a -> G.Pos -> G.Pos -> [G.Pos]
antinodes2 grid p q =
    let d = p .-. q in
    takeWhile (`G.member` grid) (iterate (.-. d) p) ++
    takeWhile (`G.member` grid) (iterate (.+. d) q)

main :: IO ()
main = pureMain $ \input -> do
    grid <- G.fromString input
    let antennas        = parseAntennas grid
        solve antinodes = S.size $ S.fromList $ do
            (_, positions) <- M.toList antennas
            (p, q)         <- pairs positions
            antinodes grid p q
    pure (pure (solve antinodes1), pure (solve antinodes2))
