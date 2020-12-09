{-# LANGUAGE LambdaCase #-}
module Main
    ( main
    ) where

import           AdventOfCode.Dijkstra (Dijkstra (..), dijkstra)
import qualified AdventOfCode.Grid     as G
import qualified AdventOfCode.V2       as V2
import qualified AdventOfCode.V2.Box   as Box
import           Control.Monad         (guard)
import           Data.Char             (isAlpha)
import qualified Data.List             as L
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe, maybeToList)
import qualified System.IO             as IO

data Portal
    = Inner !Char !Char
    | Outer !Char !Char
    deriving (Eq, Ord, Show)

insideOut :: Portal -> Portal
insideOut (Inner l r) = Outer l r
insideOut (Outer l r) = Inner l r

data Tile
    = Wall
    | Floor
    | Portal Portal
    deriving (Eq, Ord, Show)

tileToChar :: Tile -> Char
tileToChar = \case
    Wall               -> '#'
    Floor              -> '.'
    Portal (Inner c _) -> c
    Portal (Outer c _) -> c

mkGrid :: G.Grid Char -> G.Grid Tile
mkGrid grid = Map.mapMaybeWithKey
    (\pos char -> case char of
        '.' -> Just Floor
        '#' -> Just Wall
        c   -> do
            -- Here we need to take care to keep the labels in "reading" order.
            guard $ isAlpha c
            let neighbours = do
                    npos <- G.neighbours pos
                    tile <- maybeToList $ Map.lookup npos grid
                    pure (npos, tile)
            guard $ any ((== '.') . snd) neighbours
            (npos, o) <- L.find (isAlpha . snd) neighbours
            let (l, r) = if pos < npos then (c, o) else (o, c)
            pure $ Portal $ if outer npos then Outer l r else Inner l r)
    grid
  where
    (Box.Box (V2.V2 left top) (V2.V2 right bottom)) =
        fromMaybe (Box.Box G.origin G.origin) (G.box grid)

    outer (V2.V2 x y) =
        x - 2 <= left || y - 2 <= top || x + 2 >= right || y + 2 >= bottom

data Maze = Maze
    { mGrid            :: !(G.Grid Tile)
    , mPortals         :: !(Map.Map Portal G.Pos)
    , mPortalDistances :: !(Map.Map Portal [(Int, Portal)])
    }

mkMaze :: G.Grid Tile -> Maze
mkMaze grid =
    Maze {mGrid = grid, mPortals = portals, mPortalDistances = portalDistances}
  where
    portals = Map.fromList [(pid, p) | (p, Portal pid) <- Map.toList grid]

    portalDistances = Map.fromList $ do
        (portal, start) <- Map.toList portals
        let distances =
                [ (d, p)
                | (v, (d, _)) <- Map.toList $ dijkstraDistances $
                    dijkstra neighbours goal (Left start)
                , p <- case v of Left _ -> []; Right pid -> [pid]
                ]
        pure (portal, distances)

    goal                  = const False
    neighbours (Right _)  = []
    neighbours (Left pos) = do
        neighbour <- G.neighbours pos
        tile      <- maybe [] pure $ Map.lookup neighbour grid
        case tile of
            Floor      -> pure (1, Left neighbour)
            Wall       -> []
            Portal pid -> pure (0, Right pid)

shortestPath :: Maze -> Maybe Int
shortestPath (Maze _ _ portalDistances) = do
    let distances = dijkstraDistances $
            dijkstra neighbours (const False) (Outer 'A' 'A')
    pred . fst <$> Map.lookup (Outer 'Z' 'Z') distances
  where
    neighbours :: Portal -> [(Int, Portal)]
    neighbours p = do
        p' <- case p of
            Inner l r -> [Inner l r, Outer l r]
            Outer l r -> [Inner l r, Outer l r]
        fromMaybe [] $ Map.lookup p' portalDistances

recursiveShortestPath :: Maze -> Maybe Int
recursiveShortestPath (Maze _ _ portalDistances) = do
    (_, d, _) <- dijkstraGoal $ dijkstra neighbours (== goal) (0, Outer 'A' 'A')
    pure d
  where
    goal                = (0 :: Int, Outer 'Z' 'Z')
    neighbours (lvl, p) = do
        (d, p') <- fromMaybe [] $ Map.lookup p portalDistances
        let lvl' = case (p, p') of
                (Inner _ _, Outer _ _) -> lvl - 1
                (Outer _ _, Inner _ _) -> lvl + 1
                _                      -> lvl
        guard $ lvl >= 0
        pure (d, if (lvl', p') == goal then goal else (lvl', insideOut p'))

main :: IO ()
main = do
    grid <- mkGrid <$> G.readGrid pure IO.stdin
    G.printGrid IO.stderr $ fmap tileToChar grid
    print . fromMaybe 0 . shortestPath $ mkMaze grid
    print . fromMaybe 0 . recursiveShortestPath $ mkMaze grid
