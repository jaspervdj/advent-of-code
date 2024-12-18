import qualified AdventOfCode.BinarySearch as BinarySearch
import qualified AdventOfCode.Dijkstra     as Dijkstra
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import qualified AdventOfCode.NanoParser   as NP
import           AdventOfCode.V2           (V2 (..))
import           Control.Applicative       (many)
import           Control.Monad             (guard)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)

parsePositions :: NP.Parser Char [V2 Int]
parsePositions = many $
    V2 <$> (NP.decimal <* NP.char ',') <*> (NP.decimal <* NP.spaces)

prettyPosition :: V2 Int -> String
prettyPosition (V2 x y) = show x ++ "," ++ show y

type Time = Int

-- | Build a grid that stores when a snowflake will fall on that position,
-- using 0 if a snowflake will never fall there.
positionsToGrid :: Int -> [V2 Int] -> G.Grid Time
positionsToGrid n positions =
    G.generate n n $ \pos -> fromMaybe 0 $ M.lookup pos fallen
  where
    fallen = M.fromList $ zip positions [1 ..]

-- | Find a route based on a predicate that tells us whether or not we can
-- access a coordinate where a snowflake falls at some given time.  Return
-- the length of the route.
route :: (Time -> Bool) -> G.Grid Time -> Maybe Int
route accessible grid = fmap (pred . length . snd) $ Dijkstra.bfsGoal $
    Dijkstra.bfs
        (\p -> do
            q <- G.neighbours p
            guard $ maybe False accessible $ G.lookup q grid
            pure q)
        ((== V2 (G.gridWidth grid - 1) (G.gridHeight grid - 1)))
        (V2 0 0)

main :: IO ()
main = pureMain $ \str -> do
    positions <- NP.runParser parsePositions str
    let dim  = 71
        grid = positionsToGrid dim positions
        part1 = route (\t -> t <= 0 || t > 1024) grid
        part2 = fmap (positions !!) $ BinarySearch.upperBound $ \u ->
            u <$ route (\t -> t <= 0 || t > u) grid
    pure
        ( maybe (Left "no path") pure part1
        , maybe (Left "no solution") (pure . prettyPosition) part2
        )
