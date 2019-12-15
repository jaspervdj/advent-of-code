{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.V2         as V2
import qualified Data.List.Extended      as L
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe, maybeToList)
import           Data.Monoid             (Sum (..))
import           Data.OrdPSQ             (OrdPSQ)
import qualified Data.OrdPSQ             as PSQ
import qualified System.IO               as IO

data Tile = Frontier | Floor | Wall | Goal deriving (Eq, Ord, Show)

accessible :: G.Grid Tile -> G.Pos -> [(Sum Int, G.Pos)]
accessible g p =
    [ (1, q)
    | q <- G.neighbours p
    , t <- maybeToList (Map.lookup q g)
    , t /= Wall
    ]

tileToChar :: Tile -> Char
tileToChar = \case
    Frontier -> 'F'
    Floor    -> '.'
    Wall     -> '#'
    Goal     -> 'G'

dirToInt :: G.Dir -> Int
dirToInt = \case
    G.U -> 1
    G.R -> 4
    G.D -> 2
    G.L -> 3

v2ToDir :: V2.V2 Int -> G.Dir
v2ToDir (V2.V2 x y)
    | y < 0     = G.U
    | x > 0     = G.R
    | y > 0     = G.D
    | x < 0     = G.L
    | otherwise = error "bad v2ToDir"

toClosestFrontier :: G.Grid Tile -> G.Pos -> Maybe G.Dir
toClosestFrontier grid pos
    | null byDistance = Nothing
    | otherwise       = Just $ snd $ L.minimumOn fst byDistance
  where
    distances  = dijkstra (accessible grid) pos
    byDistance =
        [ (dist, v2ToDir $ step V2..-. pos)
        | (q, (dist, path)) <- Map.toList distances
        , Map.lookup q grid == Just Frontier
        , step <- take 1 $ drop 1 (reverse $ q : path)
        ]

data State = State
    { sGrid :: G.Grid Tile
    , sPos  :: G.Pos
    , sNext :: G.Dir
    }

exploration :: Program -> G.Grid Tile
exploration prog =
    -- Drop grids until it no longer contains frontiers.
    head . dropWhile (elem Frontier . map snd . Map.toList) $ map sGrid states
  where
    outputs = evalMachine $ initMachine inputs prog
    states  = L.scanl' step state0 outputs
    inputs  = map (dirToInt . sNext) states

    -- Determine next place to go.
    step state signal = state {sGrid = grid , sPos  = pos , sNext = next}
      where
        tpos = G.move (sNext state) (sPos state)
        pos  = if signal == 0 then sPos state else tpos
        next = fromMaybe G.U $ toClosestFrontier grid pos
        grid = case signal of
            0 -> Map.insert tpos Wall  (sGrid state)
            2 -> Map.insert tpos Goal  (sGrid state)
            1 -> Map.insert tpos Floor (sGrid state) <> frontier pos
            _ -> error $ "Unknown output signal: " ++ show signal

    -- Initial state.
    state0 = State grid0 G.origin G.U
    grid0  = Map.singleton G.origin Floor <> frontier G.origin

    -- Create a frontier around a tile.
    frontier p =  Map.fromList [(q, Frontier) | q <- G.neighbours p]

main :: IO ()
main = do
    program <- NP.hRunParser IO.stdin parseProgram
    let grid      = exploration program
        distances = dijkstra (accessible grid) G.origin
    goal <- case filter ((== Goal) . snd) $ Map.toList grid of
        []         -> fail "No goal found"
        (p, _) : _ -> pure p
    G.printGrid IO.stdout $ fmap tileToChar grid
    print . getSum . maybe 0 fst $ Map.lookup goal distances

    let fill = dijkstra (accessible grid) goal
    print . getSum . maximum . map (fst . snd) $ Map.toList fill

--------------------------------------------------------------------------------
-- TODO: put some decent astar in AdventOfCode.*

discover
    :: (Ord vertex, Ord distance)
    => vertex -> distance -> path
    -> OrdPSQ vertex distance path
    -> OrdPSQ vertex distance path
discover vertex distance path = snd . PSQ.alter decrease vertex
  where
    decrease (Just (d, p)) | d <= distance = ((), Just (d, p))
    decrease _             = ((), Just (distance, path))

dijkstra
    :: (Ord vertex, Ord distance, Monoid distance)
    => (vertex -> [(distance, vertex)])
    -> vertex
    -> Map vertex (distance, [vertex])
dijkstra graph start = loop Map.empty (PSQ.singleton start mempty [])
  where
    loop distances queue0 = case PSQ.minView queue0 of
        Nothing -> distances
        Just (vertex, dist, path, queue1) -> loop
            (Map.insert vertex (dist, path) distances)
            (L.foldl'
                (\q (d, n) -> discover n (dist <> d) (vertex : path) q)
                queue1
                (filter ((`Map.notMember` distances) . snd) (graph vertex)))
