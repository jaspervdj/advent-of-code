{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import qualified AdventOfCode.V2         as V2
import qualified AdventOfCode.V2.Box     as Box
import           Data.List               (find)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, isNothing)
import qualified Data.Set                as S

type StraightLines = [[G.Pos]]

parseStraightLines :: NP.Parser Char StraightLines
parseStraightLines = line `NP.sepBy1` NP.newline
  where
    line  = point `NP.sepBy1` NP.string " -> "
    point = V2 <$> NP.signedDecimal <*> (NP.char ',' *> NP.signedDecimal)

drawStraightLines :: StraightLines -> Either String (S.Set G.Pos)
drawStraightLines = go S.empty
  where
    go !acc ((p0 : p1 : ps) : sls) = do
        l <- line p0 p1
        go (acc <> S.fromList l) ((p1 : ps) : sls)
    go !acc (_ : sls) = go acc sls
    go !acc [] = Right acc

    line (V2 x0 y0) (V2 x1 y1)
        | x0 == x1  = Right [V2 x0 y | y <- [min y0 y1 .. max y0 y1]]
        | y0 == y1  = Right [V2 x y0 | x <- [min x0 x1 .. max x0 x1]]
        | otherwise = Left "not a straight line"

data Tile = Rock | Sand deriving (Eq, Show)

straightLinesToGrid :: StraightLines -> Either String (G.Grid Tile)
straightLinesToGrid = fmap (M.fromSet (const Rock)) . drawStraightLines

dropSand :: G.Pos -> G.Grid Tile -> Maybe G.Pos
dropSand pos0 grid = go pos0
  where
    go p = case find (\n -> isNothing $ M.lookup n grid) (next p) of
        Nothing -> Just p
        Just n  -> if V2.v2Y n > bottom then Nothing else go n

    next (V2 x y) = [V2 x (y + 1), V2 (x - 1) (y + 1), V2 (x + 1) (y + 1)]

    bottom = V2.v2Y $ case G.box grid of
        Nothing -> pos0
        Just b  -> Box.bBottomRight b

pourSand :: G.Pos -> G.Grid Tile -> G.Grid Tile
pourSand pos0 grid = case dropSand pos0 grid of
    Nothing            -> grid
    Just rest
        | rest == pos0 -> M.insert rest Sand grid  -- flow blocked
        | otherwise    -> pourSand pos0 (M.insert rest Sand grid)

main :: IO ()
main = pureMain $ \input -> do
    let pourx = 500
    sls <- NP.runParser parseStraightLines input
    grid1 <- pourSand (V2 pourx 0) <$> straightLinesToGrid sls
    let part1 = length . filter (== Sand) . map snd $ M.toList grid1

    let floory = (+ 2) . V2.v2Y . Box.bBottomRight . fromJust $ G.box grid1
    grid2 <- pourSand (V2 pourx 0) <$> straightLinesToGrid
        ([V2 0 floory, V2 (2 * pourx) floory] : sls)
    let part2 = length . filter (== Sand) . map snd $ M.toList grid2

    pure (pure part1, pure part2)
