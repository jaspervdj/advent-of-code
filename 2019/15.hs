{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Dijkstra   as Dijkstra
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.V2         as V2
import qualified Data.List.Extended      as L
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe, maybeToList)
import qualified System.IO               as IO

data Tile = Frontier | Floor | Wall | Goal deriving (Eq, Ord, Show)

accessible :: G.Grid Tile -> G.Pos -> [(Int, G.Pos)]
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
    distances  = Dijkstra.dijkstra (accessible grid) isGoal pos
    isGoal p   = Map.lookup p grid == Just Frontier
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
        isGoal p  = Map.lookup p grid == Just Goal
        distances = Dijkstra.dijkstra (accessible grid) isGoal G.origin
    goal <- case filter ((== Goal) . snd) $ Map.toList grid of
        []         -> fail "No goal found"
        (p, _) : _ -> pure p
    G.printGrid IO.stdout $ fmap tileToChar grid
    print . maybe 0 fst $ Map.lookup goal distances

    let fill = Dijkstra.dijkstra (accessible grid) (const False) goal
    print . maximum . map (fst . snd) $ Map.toList fill
