import qualified AdventOfCode.Dijkstra     as Dijkstra
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..))
import           Data.Char                 (digitToInt, isDigit)
import           Data.Maybe                (maybeToList)
import qualified Data.Set                  as S

parseGrid :: String -> Either String (G.Grid Int)
parseGrid str = G.fromString str >>= traverse parseCell
  where
    parseCell c
        | isDigit c = pure $ digitToInt c
        | otherwise = Left $ "bad char: " ++ show c

type Turning  = Int -> G.Dir -> [G.Dir]
data Crucible = Init | Rolling G.Pos G.Dir Int deriving (Eq, Ord)

neighbours :: Turning -> G.Grid Int -> Crucible -> [(Int, Crucible)]
neighbours _ grid Init = do
    dir <- [G.D, G.R]
    let pos = G.move 1 dir (V2 0 0)
    pure (grid G.! pos, Rolling pos dir 1)
neighbours turning grid (Rolling p0 d0 n0) = do
    d1 <- turning n0 d0
    let p1 = G.move 1 d1 p0
    heat <- maybeToList $ G.lookup p1 grid
    pure (heat, Rolling p1 d1 (if d1 == d0 then n0 + 1 else 1))

main :: IO ()
main = pureMain $ \str -> do
    grid <- parseGrid str
    let dest = V2 (G.gridWidth grid - 1) (G.gridHeight grid - 1)

        solve turning = Dijkstra.goal $ Dijkstra.dijkstra Dijkstra.Options
            { Dijkstra.neighbours = neighbours turning grid
            , Dijkstra.find = Dijkstra.FindOne (\crucible -> case crucible of
                Rolling pos _ _ -> pos == dest
                _               -> False)
            , Dijkstra.start = S.singleton Init
            }
        heat solution = case solution of
            Just (h, _) -> pure h
            _           -> Left $ "no solution"

        turning1 n dir = [G.turnLeft dir, G.turnRight dir] ++ [dir | n < 3]
        turning2 n dir
            | n < 4     = [dir]
            | n >= 10   = [G.turnLeft dir, G.turnRight dir]
            | otherwise = [dir, G.turnLeft dir, G.turnRight dir]

    pure (heat (solve turning1), heat (solve turning2))
