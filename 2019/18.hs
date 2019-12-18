{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Main where

import qualified AdventOfCode.Dijkstra as Dijkstra
import qualified AdventOfCode.Grid     as G
import           Data.Char             (isAlpha, isLower, toLower)
import qualified Data.Map              as Map
import           Data.Maybe            (listToMaybe, mapMaybe)
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
    { ssPos  :: !G.Pos
    , ssKeys :: !(Set.Set Char)
    } deriving (Eq, Ord, Show)

collectAllKeys :: Maze -> Maybe Int
collectAllKeys maze =
    let distances = Dijkstra.dijkstra neighbours goal start in
    listToMaybe . map (fst . snd) . filter (goal . fst) $ Map.toList distances
  where
    neighbours :: SearchState -> [(Int, SearchState)]
    neighbours ss = map ((,) 1) $ mapMaybe (move ss) [G.U, G.R, G.D, G.L]

    goal :: SearchState -> Bool
    goal = (>= numKeysInMaze) . Set.size . ssKeys

    numKeysInMaze :: Int
    numKeysInMaze = length [() | (_, Key _) <- Map.toList maze]

    start :: SearchState
    start = SearchState
        { ssPos = case [p | (p, Start) <- Map.toList maze] of
            [p] -> p
            _   -> error "ambiguous or empty starting position"
        , ssKeys = Set.empty
        }

    move :: SearchState -> G.Dir -> Maybe SearchState
    move !ss dir = case Map.lookup pos maze of
        Just Wall -> Nothing
        Just (Door k) | not (k `Set.member` ssKeys ss) -> Nothing
        Just (Key k) -> Just ss {ssPos = pos, ssKeys = Set.insert k (ssKeys ss)}
        _ -> Just ss {ssPos = pos}
      where
        pos = G.move dir (ssPos ss)

main :: IO ()
main = do
    maze <- G.readGrid (either fail pure . charToTile) IO.stdin
    print $ collectAllKeys maze
