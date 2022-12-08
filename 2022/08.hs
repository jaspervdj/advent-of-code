{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..))
import           Control.Monad             ((>=>))
import           Data.Char                 (digitToInt, isDigit)
import           Data.List                 (foldl')
import qualified Data.Set                  as S

parseTrees :: String -> Either String (G.Grid Int)
parseTrees = G.fromString >=> traverse parseTree
  where
    parseTree c = if isDigit c then Right (digitToInt c) else Left (show c)

part1 :: G.Grid Int -> S.Set G.Pos
part1 g = foldl' (\acc -> snd . foldl' step (-1, acc)) S.empty $
    up ++ down ++ left ++ right
  where
    step !(!h, !acc) pos =
        let tree = G.index pos g in
        if tree > h then (tree, S.insert pos acc) else (h, acc)

    up    = [[V2 x y | y <- [ymax, ymax - 1 .. 0]] | x <- [0 .. xmax]]
    down  = [[V2 x y | y <- [0 .. ymax]] | x <- [0 .. xmax]]
    left  = [[V2 x y | x <- [xmax, xmax - 1 .. 0]] | y <- [0 .. ymax]]
    right = [[V2 x y | x <- [0 .. xmax]] | y <- [0 .. ymax]]

    (xmax, ymax) = (G.gridWidth  g - 1, G.gridHeight g - 1)

scenicScore :: G.Grid Int -> G.Pos -> Int
scenicScore grid pos0 =
    score up 0 pos0 * score down 0 pos0 * score left 0 pos0 * score right 0 pos0
  where
    height0 = G.index pos0 grid

    score next !acc pos =
        let pos' = next pos in
        case G.lookup pos' grid of
            Just tree | tree < height0 -> score next (acc + 1) pos'
                      | otherwise      -> acc + 1
            _                          -> acc

    up    (V2 x y) = V2 x       (y - 1)
    down  (V2 x y) = V2 x       (y + 1)
    left  (V2 x y) = V2 (x - 1) y
    right (V2 x y) = V2 (x + 1) y

main :: IO ()
main = pureMain $ \input -> do
    trees <- parseTrees input
    let part2 = maximum . map (scenicScore trees . fst) $ G.toList trees
    pure (pure (S.size (part1 trees)), pure part2)
