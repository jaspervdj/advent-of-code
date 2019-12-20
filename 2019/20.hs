{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Dijkstra as Dijkstra
import qualified AdventOfCode.Grid     as G
import           Control.Monad         (guard)
import           Data.Char             (isAlpha)
import qualified Data.List             as L
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe, listToMaybe, maybeToList)
import qualified System.IO             as IO

data PortalId = PortalId !Char !Char deriving (Eq, Ord, Show)

data Tile
    = Wall
    | Floor
    | Portal PortalId
    deriving (Eq, Ord, Show)

tileToChar :: Tile -> Char
tileToChar = \case
    Wall                  -> '#'
    Floor                 -> '.'
    Portal (PortalId c _) -> c

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
            pure $ Portal $ if pos < npos then PortalId c o else PortalId o c)
    grid

data Maze = Maze
    { mGrid    :: !(G.Grid Tile)
    , mPortals :: !(Map.Map PortalId [G.Pos])
    }

mkMaze :: G.Grid Tile -> Maze
mkMaze grid = Maze grid $
    Map.fromListWith (++) [(pid, [p]) | (p, Portal pid) <- Map.toList grid]

portalTiles :: Maze -> PortalId -> [G.Pos]
portalTiles (Maze grid portals) pid = do
    pos       <- fromMaybe [] (Map.lookup pid portals)
    neighbour <- G.neighbours pos
    guard $ Map.lookup neighbour grid == Just Floor
    pure neighbour

shortestPath :: Maze -> Maybe Int
shortestPath maze@(Maze grid _) = do
    start <- listToMaybe . portalTiles maze $ PortalId 'A' 'A'
    end   <- listToMaybe . portalTiles maze $ PortalId 'Z' 'Z'
    let distances = Dijkstra.dijkstra neighbours (const False) start
    fst <$> Map.lookup end distances
  where
    neighbours :: G.Pos -> [(Int, G.Pos)]
    neighbours pos = do
        neighbour <- G.neighbours pos
        tile      <- maybe [] pure $ Map.lookup neighbour grid
        case tile of
            Floor      -> pure (1, neighbour)
            Wall       -> []
            Portal pid -> do
                warp <- portalTiles maze pid
                guard $ warp /= pos
                pure (1, warp)

main :: IO ()
main = do
    grid <- mkGrid <$> G.readGrid pure IO.stdin
    print . fromMaybe 0 . shortestPath $ mkMaze grid
