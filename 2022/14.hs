{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import qualified AdventOfCode.V2         as V2
import qualified AdventOfCode.V2.Box     as Box
import           Data.Foldable           (find, foldl', toList)
import qualified Data.Map                as M
import           Data.Maybe              (isNothing)
import qualified Data.Set                as S

type StraightLines = [[G.Pos]]

parseStraightLines :: NP.Parser Char StraightLines
parseStraightLines = toList <$> line `NP.sepBy1` NP.newline
  where
    line  = fmap toList $ point `NP.sepBy1` NP.string " -> "
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

data Cave = Cave {caveGrid :: !(G.Grid Tile), caveBottom :: !Int}

instance Show Cave where
    show = G.toString . fmap showTile . caveGrid
      where
        showTile Rock = '#'
        showTile Sand = 'o'

caveSand :: Cave -> Int
caveSand = length . filter (== Sand) . map snd . M.toList . caveGrid

straightLinesToCave :: StraightLines -> Either String Cave
straightLinesToCave sls = do
    rocks <- drawStraightLines sls
    let grid   = M.fromSet (const Rock) rocks
        bottom = maybe 0 (V2.v2Y . Box.bBottomRight) $ G.box grid
    pure Cave {caveGrid = grid, caveBottom = bottom}

fall :: G.Pos -> [G.Pos]
fall (V2 x y) = [V2 x (y + 1), V2 (x - 1) (y + 1), V2 (x + 1) (y + 1)]

dropSand :: G.Pos -> Cave -> Maybe G.Pos
dropSand p0 cave = go p0
  where
    grid = caveGrid cave
    go p = case find (\n -> isNothing $ M.lookup n grid) (fall p) of
        Nothing -> Just p
        Just n  -> if V2.v2Y n > caveBottom cave then Nothing else go n

pourSand :: G.Pos -> Cave -> Cave
pourSand p cave = case dropSand p cave of
    Nothing   -> cave
    Just rest -> pourSand p cave {caveGrid = M.insert rest Sand (caveGrid cave)}

fillSand :: G.Pos -> Cave -> Cave
fillSand p0 cave = cave {caveGrid = go (caveGrid cave) p0}
  where
    go !grid p@(V2 _ y)
        | y >= caveBottom cave = grid
        | p `M.member` grid    = grid
        | otherwise            = foldl' go (M.insert p Sand grid) (fall p)

main :: IO ()
main = pureMain $ \input -> do
    sls   <- NP.runParser parseStraightLines input
    cave0 <- straightLinesToCave sls

    let cave1 = pourSand (V2 500 0) cave0
        cave2 = fillSand (V2 500 0) cave0 {caveBottom = caveBottom cave0 + 2}

    pure (pure (caveSand cave1), pure (caveSand cave2))
