import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           Data.Char                 (digitToInt, isDigit)
import           Data.Foldable             (fold)
import qualified Data.Map                  as M
import           Data.Maybe                (mapMaybe)
import           Data.Monoid               (Sum (..))
import qualified Data.Set                  as S
import           Data.Traversable          (for)

height :: Int -> G.Grid Int -> [G.Pos]
height h = map fst . filter ((== h) . snd) . G.toList

hikes :: Monoid m => (G.Pos -> m) -> G.Grid Int -> M.Map G.Pos m
hikes f grid = ascend 1 $ M.fromList [(p, f p) | p <- height 0 grid]
  where
    ascend h level | h > 9 = level
    ascend h level         = ascend (h + 1) $ M.fromList
       [ (p, mconcat $ mapMaybe (`M.lookup` level) $ G.neighbours p)
       | p <- height h grid
       ]

main :: IO ()
main = pureMain $ \input -> do
    grid0 <- G.fromString input
    grid1 <- for grid0 $ \c ->
        if isDigit c then pure (digitToInt c) else Left $ "bad char: " ++ [c]
    let part1 = getSum $ foldMap (Sum . S.size) $ hikes S.singleton grid1
        part2 = getSum $ fold $ hikes (\_ -> Sum 1) grid1 :: Int
    pure (pure part1, pure part2)
