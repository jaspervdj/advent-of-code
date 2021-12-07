module Main where

import qualified AdventOfCode.Grid         as G
import qualified AdventOfCode.Grid.Bounded as GB
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..), (.+.))
import qualified AdventOfCode.V2.Box       as Box
import           Control.Monad             (guard, (>=>))
import qualified Data.List                 as L
import           Data.List.Extra           (select, stripSuffix)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe, maybeToList)
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import           Text.Read                 (readMaybe)


border :: GB.Dir -> GB.Grid a -> V.Vector a
border dir g = case dir of
    GB.U -> V.generate w $ \x -> GB.index (V2 x       0)       g
    GB.R -> V.generate h $ \y -> GB.index (V2 (w - 1) y)       g
    GB.D -> V.generate w $ \x -> GB.index (V2 x       (h - 1)) g
    GB.L -> V.generate h $ \y -> GB.index (V2 0       y)       g
  where
    (w, h) = (GB.gridWidth g, GB.gridHeight g)


rotateClockwise :: GB.Grid a -> GB.Grid a
rotateClockwise g = GB.generate h w $ \(V2 x y) -> GB.index (V2 y (h - x - 1)) g
  where
    (w, h) = (GB.gridWidth g, GB.gridHeight g)


flipHorizontal :: GB.Grid a -> GB.Grid a
flipHorizontal g = GB.generate w h $ \(V2 x y) -> GB.index (V2 (w - x - 1) y) g
  where
    (w, h) = (GB.gridWidth g, GB.gridHeight g)


transformations :: GB.Grid a -> [GB.Grid a]
transformations grid0 =
    let grid1     = rotateClockwise grid0
        grid2     = rotateClockwise grid1
        grid3     = rotateClockwise grid2
        rotations = [grid0, grid1, grid2, grid3] in
    rotations ++ map flipHorizontal rotations


parseTiles :: String -> Either String [(Int, GB.Grid Char)]
parseTiles = go . lines
  where
    parseTileId = L.stripPrefix "Tile " >=> stripSuffix ":" >=> readMaybe
    go input = case break null input of
        ([], []) -> Right []
        (td : gridLines, remainder) | Just n <- parseTileId td -> do
            grid <- GB.fromString $ unlines gridLines
            ((n, grid) :) <$> go (dropWhile null remainder)
        e -> Left $ "parse error: " ++ show e


fitTile
    :: Eq a
    => (Int, GB.Grid a) -> G.Grid (Int, GB.Grid a)
    -> [G.Grid (Int, GB.Grid a)]
fitTile tile grid | Map.null grid = [Map.singleton (V2 0 0) tile]
fitTile (tid, tile) grid = do
    hole <- holes
    transformed <- transformations tile
    guard $ borderCheck hole transformed
    pure $ Map.insert hole (tid, transformed) grid
  where
    holes = filter (not . (`Map.member` grid)) $
        concatMap G.neighbours $ Map.keys grid

    borderCheck pos t = and $ do
        dir <- [minBound .. maxBound]
        let nbPos = G.move 1 dir pos
            nbDir = G.turnAround dir
        (_, nbTile) <- maybeToList $ Map.lookup nbPos grid
        pure $ border dir t == border nbDir nbTile


fitTiles :: Eq a => [(Int, GB.Grid a)] -> [G.Grid (Int, GB.Grid a)]
fitTiles = go Map.empty
  where
    go acc [] = [acc]
    go acc tiles = do
        (tile, remainder) <- select tiles
        acc' <- take 1 $ fitTile tile acc
        go acc' remainder


corners :: G.Grid a -> [a]
corners g = catMaybes $
    let (xs, ys)     = unzip [(x, y) | V2 x y <- Map.keys g]
        (minx, maxx) = (minimum xs, maximum xs)
        (miny, maxy) = (minimum ys, maximum ys) in
    [ Map.lookup (V2 minx miny) g, Map.lookup (V2 minx maxy) g
    , Map.lookup (V2 maxx miny) g, Map.lookup (V2 maxx maxy) g
    ]


merge :: G.Grid (GB.Grid a) -> GB.Grid a
merge grid = GB.generate (Box.width box * tileW) (Box.height box * tileH) $
    \(V2 x y) ->
        let (bx, tx) = divMod x tileW
            (by, ty) = divMod y tileH
            tile = grid Map.! (Box.bTopLeft box .+. V2 bx by) in
        GB.index (V2 tx ty .+. V2 1 1) tile
  where
    box = fromMaybe (error "merge: empty") $ G.box grid
    (tileW, tileH) = case Map.minView grid of
        Just (g, _) -> (GB.gridWidth g - 2, GB.gridHeight g - 2)
        _           -> error "merge: empty"


findMonsterTiles :: GB.Grid Bool -> GB.Grid Bool -> Set.Set (V2 Int)
findMonsterTiles needle0 haystack = Set.unions $ do
    needle <- transformations needle0
    ox <- [0 .. GB.gridWidth haystack - GB.gridWidth needle]
    oy <- [0 .. GB.gridHeight haystack - GB.gridHeight needle]
    let tiles = Set.fromList $ do
            x <- [0 .. GB.gridWidth needle - 1]
            y <- [0 .. GB.gridHeight needle - 1]
            guard $ GB.index (V2 x y) needle
            pure $ V2 x y .+. V2 ox oy
    guard $ all (`GB.index` haystack) tiles
    pure tiles


main :: IO ()
main = pureMain $ \inputstr -> do
    tiles <- parseTiles inputstr
    seaMonster <- fmap (fmap (== '#')) . GB.fromString $ unlines
        [ "                  # "
        , "#    ##    ##    ###"
        , " #  #  #  #  #  #   "
        ]

    solution <- case fitTiles tiles of
        s : _ -> Right s
        []    -> Left "no solution"

    let big = fmap (== '#') . merge $ fmap snd solution
        habitat = length . filter snd $ GB.toList big
    pure
        ( pure . product . map fst $ corners solution
        , pure $ habitat - Set.size (findMonsterTiles seaMonster big)
        )
