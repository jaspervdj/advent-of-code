import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main (simpleMain)
import           AdventOfCode.V2   (v2Y)
import           Data.List         (sortOn)
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Data.Ord          (Down (..))
import qualified Data.Set          as S

part1 :: S.Set G.Pos -> G.Pos -> Int
part1 splitters start = go 0 (S.singleton start)
  where
    lowest = maximum $ map v2Y $ S.toList splitters

    beam :: G.Pos -> (Int, [G.Pos])
    beam p
        | q `S.member` splitters = (1, [G.move 1 G.L q, G.move 1 G.R q])
        | otherwise              = (0, [q])
      where
        q = G.move 1 G.D p

    go :: Int -> S.Set G.Pos -> Int
    go !acc ps
        | all ((> lowest) . v2Y) (S.toList ps) = acc
        | otherwise = go (acc + sum splits) (S.fromList $ concat qs)
      where
        (splits, qs) = unzip $ map beam $ S.toList ps

part2 :: S.Set G.Pos -> G.Pos -> Int
part2 splitters start = fromMaybe 1 $ do
    s <- next start
    M.lookup s $ go ascending M.empty
  where
    lowest    = maximum $ map v2Y $ S.toList splitters
    ascending = sortOn (Down . v2Y) $ S.toList splitters

    next :: G.Pos -> Maybe G.Pos  -- Find next splitter
    next p
        | v2Y p > lowest         = Nothing
        | p `S.member` splitters = Just p
        | otherwise              = next $ G.move 1 G.D p

    go  :: [G.Pos]          -- ^ Splitters to process
        -> M.Map G.Pos Int  -- ^ Timelines per splitter
        -> M.Map G.Pos Int  -- ^ Result
    go [] acc = acc
    go (p : ps) acc =
        let splits = sum $ do
                q <- [G.move 1 d p | d <- [G.L, G.R]]
                pure $ fromMaybe 1 $ do
                    s <- next q
                    M.lookup s acc in
        go ps $ M.insert p splits acc

main :: IO ()
main = simpleMain $ \str ->
    let grid = G.fromString str
        splitters = M.keysSet $ M.filter (== '^') grid
        start = case [p | (p, 'S') <- M.toList grid] of
            [p] -> p
            _   -> error "start not found" in
    (part1 splitters start, part2 splitters start)
