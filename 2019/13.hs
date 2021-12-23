{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.V2         as V2
import           Data.Function           (on)
import qualified Data.List               as L
import qualified Data.Map                as Map
import           Data.Maybe              (mapMaybe)
import qualified System.IO               as IO

triples :: [a] -> [(a, a, a)]
triples (x : y : z : l) = (x, y, z) : triples l
triples _               = []

data Tile
    = Empty
    | Wall
    | Block
    | Paddle
    | Ball
    deriving (Eq, Ord, Show)

intToTile :: Int -> Maybe Tile
intToTile = \case
    0 -> Just Empty
    1 -> Just Wall
    2 -> Just Block
    3 -> Just Paddle
    4 -> Just Ball
    _ -> Nothing

tileToChar :: Tile -> Char
tileToChar = \case
    Empty  -> ' '
    Wall   -> '+'
    Block  -> '#'
    Paddle -> '-'
    Ball   -> 'o'

parseTiles :: [Int] -> G.Grid Tile
parseTiles = Map.fromList . mapMaybe parseTile . triples
  where
    parseTile (x, y, t) = (,) (V2.V2 x y) <$> intToTile t

data Game = Game
    { gScore  :: !Int
    , gGrid   :: !(G.Grid Tile)
    , gBall   :: !G.Pos
    , gPaddle :: !G.Pos
    }

updateGame :: (Int, Int, Int) -> Game -> Game
updateGame (-1, 0, score) g = g {gScore = score}
updateGame (x, y, i)      g = case intToTile i of
    Nothing -> g
    Just t  -> g
        { gGrid   = Map.insert (V2.V2 x y) t (gGrid g)
        , gBall   = if t == Ball   then V2.V2 x y else gBall g
        , gPaddle = if t == Paddle then V2.V2 x y else gPaddle g
        }

simGame :: Program -> Int
simGame prog =
    go (Game 0 Map.empty G.origin G.origin) (initMachine [] prog)
  where
    go g m = case int of
        HaltSuccess       -> gScore g'
        InsufficientInput -> go g' m' {mInputs = [input]}
        _                 -> error $ show int
      where
        (outs, int, m') = runMachine m
        g'              = L.foldl' (flip updateGame) g (triples outs)
        input           = case on compare V2.v2X (gPaddle g') (gBall g') of
            LT -> 1
            EQ -> 0
            GT -> -1

main :: IO ()
main = do
    prog1 <- NP.hRunParser IO.stdin parseProgram
    let grid1 = parseTiles $ evalMachine $ initMachine [] prog1
    print . length . filter (== Block) . map snd $ Map.toList grid1
    print $ simGame $ makeProgram [2] <> prog1
