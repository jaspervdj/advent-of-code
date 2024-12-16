{-# LANGUAGE LambdaCase #-}
module Main
    ( main
    ) where

import qualified AdventOfCode.Dijkstra as Dijkstra
import qualified AdventOfCode.Grid     as G
import qualified AdventOfCode.V2       as V2
import           Data.Char             (isAlpha, isLower, toLower)
import qualified Data.Map              as Map
import           Data.Maybe            (mapMaybe)
import qualified Data.Set              as Set
import qualified System.IO             as IO

data Tile
    = Wall
    | Floor
    | Start
    | Door Char
    | Key Char
    deriving (Show)

type Maze = G.Grid Tile

charToTile :: Char -> Either String Tile
charToTile = \case
    '#'            -> pure Wall
    '.'            -> pure Floor
    '@'            -> pure Start
    d  | isAlpha d -> pure $ if isLower d then Key d else Door (toLower d)
    c              -> Left $ "Unknown map character: " ++ show c

data SearchState = SearchState
    { ssRobots :: !(Map.Map Int G.Pos)
    , ssKeys   :: !(Set.Set Char)
    } deriving (Eq, Ord, Show)

collectAllKeys :: Maze -> Maybe Int
collectAllKeys maze = do
    (dist, _) <- Dijkstra.goal $ Dijkstra.dijkstra Dijkstra.Options
        { Dijkstra.neighbours = neighbours
        , Dijkstra.find       = Dijkstra.FindOne goal
        , Dijkstra.start      = Set.fromList [start]
        }
    pure dist
  where
    neighbours :: SearchState -> [(Int, SearchState)]
    neighbours (SearchState robots keys) = do
        (idx, pos) <- Map.toList robots
        (d, pos', key) <- walkToNewKey keys pos
        let keys'   = Set.insert key keys
            robots' = Map.insert idx pos' robots
        pure (d, SearchState robots' keys')

    goal :: SearchState -> Bool
    goal = (>= numKeysInMaze) . Set.size . ssKeys

    numKeysInMaze :: Int
    numKeysInMaze = length [() | (_, Key _) <- Map.toList maze]

    start :: SearchState
    start = SearchState
        (Map.fromList $ zip [0 ..] [p | (p, Start) <- Map.toList maze])
        Set.empty

    moves :: Set.Set Char -> G.Pos -> [(G.Pos, Maybe Char)]
    moves keys p = G.neighbours p >>= \p' -> case Map.lookup p' maze of
        Just Wall                                 -> []
        Just (Door k) | not (k `Set.member` keys) -> []
        Just (Key k)  | not (k `Set.member` keys) -> [(p', Just k)]
        _                                         -> [(p', Nothing)]

    walkToNewKey :: Set.Set Char -> G.Pos -> [(Int, G.Pos, Char)]
    walkToNewKey keys pos =
        -- A nested dijkstra so we only look at interesting positions (those
        -- that have keys).
        mapMaybe (\((p, mbK), (d, _)) -> mbK >>= \k -> pure (d, p, k)) .
        Map.toList $ Dijkstra.back $ Dijkstra.dijkstra Dijkstra.Options
            { Dijkstra.neighbours = (\case
                (_, Just _)  -> []
                (p, Nothing) -> map ((,) 1) (moves keys p))
            , Dijkstra.find = Dijkstra.NoFind
            , Dijkstra.start = Set.singleton (pos, Nothing)
            }

parallelize :: Maze -> Maybe Maze
parallelize maze = case [p | (p, Start) <- Map.toList maze] of
    [V2.V2 x y] -> Just $ Map.fromList
        [ (V2.V2 (x - 1) (y - 1), Start)
        , (V2.V2  x      (y - 1), Wall)
        , (V2.V2 (x + 1) (y - 1), Start)
        , (V2.V2 (x - 1)  y     , Wall)
        , (V2.V2  x       y     , Wall)
        , (V2.V2 (x + 1)  y     , Wall)
        , (V2.V2 (x - 1) (y + 1), Start)
        , (V2.V2  x      (y + 1), Wall)
        , (V2.V2 (x + 1) (y + 1), Start)
        ] <> maze
    _ -> Nothing

main :: IO ()
main = do
    maze    <- G.readGrid (either fail pure . charToTile) IO.stdin
    print $ collectAllKeys maze
    patched <- maybe (fail "parallelize failed") pure (parallelize maze)
    print $ collectAllKeys patched
