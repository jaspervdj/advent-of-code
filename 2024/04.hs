import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           (V2 (..), (.+.), (.-.))
import           Control.Monad             (guard)
import           Data.List                 (isPrefixOf, unfoldr)
import           Data.Maybe                (maybeToList)

findLine :: G.Grid a -> [[a]]
findLine grid = do
    (pos, _) <- G.toList grid
    delta <- fmap (.-. pos) $ G.neighbours pos ++ G.diagonal pos
    pure $ unfoldr (\p -> (, p .+. delta) <$> G.lookup p grid) pos

data X a = X a a a a a deriving (Show)  -- Top to bottom, left to right

findX :: G.Grid a -> [X a]
findX grid = do
    (pos, c) <- G.toList grid
    maybeToList $ X
        <$> G.lookup (pos .+. V2 (-1)   1 ) grid
        <*> G.lookup (pos .+. V2   1    1 ) grid
        <*> pure c
        <*> G.lookup (pos .+. V2 (-1) (-1)) grid
        <*> G.lookup (pos .+. V2   1  (-1)) grid

main :: IO ()
main = pureMain $ \input -> do
    grid <- G.fromString input
    let part1 = length $ filter ("XMAS" `isPrefixOf`) $ findLine grid
        part2 = length $ do
            x@(X tl tr c bl br) <- findX grid
            guard $ [tl, c, br] == "MAS" || [br, c, tl] == "MAS"
            guard $ [tr, c, bl] == "MAS" || [bl, c, tr] == "MAS"
            pure x
    pure (pure part1, pure part2)
