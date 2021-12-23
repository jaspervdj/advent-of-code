{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified AdventOfCode.Dijkstra as Dijkstra
import qualified AdventOfCode.Grid     as G
import           AdventOfCode.Main     (simpleMain)
import           AdventOfCode.V2       (V2 (..))
import           Control.Monad         (guard)
import           Data.Bifunctor        (second)
import           Data.Char             (isUpper)
import qualified Data.Map              as M
import           Data.Maybe            (isNothing)
import qualified Data.Set              as S

type Energy = Int
type Amphipod = Int

data Tile = Hallway | Room Char deriving (Eq, Show)

isRoom :: Tile -> Bool
isRoom = not . (== Hallway)

data Env = Env
    { envTypes :: M.Map Amphipod Char
    , envGrid  :: G.Grid Tile
    } deriving (Show)

data State = State
    { statePositions :: M.Map Amphipod G.Pos
    , stateMoved     :: M.Map Amphipod Int
    } deriving (Eq, Ord, Show)

energy :: Char -> Int
energy 'A' = 1
energy 'B' = 10
energy 'C' = 100
energy 'D' = 1000
energy _   = 0

entrance :: Env -> G.Pos -> Bool
entrance Env{..} pos =
    M.lookup pos envGrid == Just Hallway &&
    any (\p -> maybe False isRoom $ M.lookup p envGrid) (G.neighbours pos)

reachable :: Env -> State -> Amphipod -> [(G.Pos, Energy)]
reachable env@Env{..} State{..} amphi =
    map (second ((* energy typ) . pred . length)) .
    filter (\(p, _) -> p /= start && not (entrance env p) && fills p) .
    M.toList . Dijkstra.bfsDistances $
    Dijkstra.bfs neighbours (const False) start
  where
    start = statePositions M.! amphi
    typ = envTypes M.! amphi
    neighbours pos = do
        nb <- G.neighbours pos
        guard $ case M.lookup nb envGrid of
            Nothing       -> False
            Just Hallway  -> True
            Just (Room c) -> c == typ || Just (Room c) == M.lookup pos envGrid
        guard $ nb `notElem` statePositions
        pure nb

    fills pos = case envGrid M.! pos of
        Hallway -> True
        -- Check that we nicely move to the bottom
        Room c -> (== 1) $ length $
            filter (\p -> p `notElem` statePositions || p == start) $
            filter (`M.member` envGrid) $
            G.neighbours pos

stateGoal :: Env -> State -> Bool
stateGoal Env{..} State{..} = and $ do
    (amphi, pos) <- M.toList statePositions
    pure $ envGrid M.! pos == Room (envTypes M.! amphi)

stateNeighbours :: Env -> State -> [(Energy, State)]
stateNeighbours env@Env{..} state@State{..} = do
    (amphi, _start) <- M.toList statePositions
    let moved = stateMoved M.! amphi
    guard $ moved < 2
    (end, cost) <- reachable env state amphi
    guard $ case envGrid M.! end of
        Hallway -> moved == 0
        Room _  -> True
    let state' = state
            { stateMoved     = M.insertWith (+) amphi 1 stateMoved
            , statePositions = M.insert amphi end statePositions
            }
    pure (cost, state')

solve :: Env -> State -> Energy
solve e s = case Dijkstra.dijkstraGoal d of
    Nothing        -> 0
    Just (_, c, _) -> c
  where
    d = Dijkstra.dijkstra (stateNeighbours e) (stateGoal e) s

parseInput :: String -> (Env, State)
parseInput str =
    let grid0 = G.fromString str
        envGrid = flip M.mapMaybeWithKey grid0 $ \pos -> \case
            '.' -> Just Hallway
            c | isUpper c -> case v2X pos of
                3 -> Just $ Room 'A'
                5 -> Just $ Room 'B'
                7 -> Just $ Room 'C'
                9 -> Just $ Room 'D'
                _ -> Nothing
            _ -> Nothing
        envTypes = (grid0 M.!) <$> statePositions
        statePositions = M.fromList $ zip [0 ..]
            [pos | (pos, c) <- M.toList grid0, isUpper c]
        stateMoved = 0 <$ statePositions in
    (Env {..}, State {..})

main :: IO ()
main = simpleMain $ \input ->
    let (env, state) = parseInput input in
    (show (env, state), solve env state)
