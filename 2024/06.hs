import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           Control.Monad             (guard)
import qualified Data.Set                  as S

data State = State {sPos :: G.Pos, sDir :: G.Dir} deriving (Eq, Ord, Show)
data End = Loop | LeftMap deriving (Eq)

walk :: G.Grid Bool -> State -> (S.Set State, End)
walk grid = go S.empty
  where
    go visited s@(State pos dir) = case G.lookup pos' grid of
        _ | s `S.member` visited -> (visited, Loop)
        Nothing                  -> (visited', LeftMap)
        Just True                -> go visited' $ State pos $ G.turnRight dir
        Just False               -> go visited' $ State pos' dir
      where
        pos'     = G.move 1 dir pos
        visited' = S.insert s visited

main :: IO ()
main = pureMain $ \input -> do
    grid0 <- G.fromString input
    initialState <- case filter ((== '^') . snd) $ G.toList grid0 of
        [(pos, _)] -> pure $ State pos G.U
        _          -> Left "initial position not found"

    let grid1        = fmap (== '#') grid0
        (visited, _) = walk grid1 initialState
        positions    = S.map sPos visited

    let part2 = length $ do
            block <- S.toList $ S.delete (sPos initialState) positions
            let grid     = G.mapWithKey (\p v -> p == block || v) grid1
                (_, end) = walk grid initialState
            guard $ end == Loop
            pure ()

    pure (pure (S.size positions), pure part2)
