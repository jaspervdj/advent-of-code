{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Dijkstra as Dijkstra
import           AdventOfCode.Main     (simpleMain)
import           Control.Monad         (guard)
import           Data.Char             (isUpper)
import           Data.List             (transpose)
import           Data.Maybe            (isJust, isNothing)
import qualified Data.Set              as S
import qualified Data.Vector           as V

type Amphipod = Char
type Room = [Amphipod]
type Hallway = V.Vector (Maybe Amphipod)
type State = (Hallway, V.Vector Room)

parseRooms :: String -> V.Vector Room
parseRooms =
    V.fromList . transpose . filter (not . null) . map (filter isUpper) . lines

parseState :: String -> State
parseState input = (V.replicate 11 Nothing, parseRooms input)

desired :: V.Vector Char
desired = V.fromList "ABCD"

entrances :: V.Vector Int
entrances = V.fromList [2, 4, 6, 8]

energy :: Char -> Int
energy = \case
    'A' -> 1
    'B' -> 10
    'C' -> 100
    'D' -> 1000
    _   -> 0

walkable :: Hallway -> Int -> [Int]
walkable hw x0 = x0 : free pred (x0 - 1) <> free succ (x0 + 1)
  where
    free f x
        | x < 0 || x >= V.length hw = []
        | isJust (hw V.! x)         = []
        | otherwise                 = x : free f (f x)

pop :: Int -> State -> V.Vector (Int, State)
pop roomSize (hw, rms) = do
    (i, (x, correct, stack)) <- V.indexed $ V.zip3 entrances desired rms
    guard $ any (/= correct) stack
    x' <- V.fromList $ filter (`notElem` entrances) $ walkable hw x
    let steps = (roomSize - length stack + 1) + abs (x' - x)
    pure
        ( steps * energy (head stack)
        , (hw V.// [(x', Just (head stack))], rms V.// [(i, tail stack)])
        )

push :: Int -> State -> V.Vector (Int, State)
push roomSize (hw, rms) = do
    (x, Just amphi) <- V.indexed hw
    (i, (x', correct, stack)) <- V.indexed $ V.zip3 entrances desired rms
    guard $ amphi == correct && all (== correct) stack
    let hw' = hw V.// [(x, Nothing)]
    guard $ x' `elem` walkable hw' x
    let steps = abs (x' - x) + (roomSize - length stack)
    pure
        ( steps * energy amphi
        , (hw', rms V.// [(i, amphi : stack)])
        )

done :: State -> Bool
done (hw, rms) =
    V.all isNothing hw &&
    V.and (V.zipWith (\amphi rm -> all (== amphi) rm) desired rms)

solve :: Int -> State -> Int
solve roomSize state =
    case Dijkstra.goal (Dijkstra.dijkstra opts) of
        Nothing     -> 0
        Just (d, _) -> d
  where
    opts = Dijkstra.Options
        { Dijkstra.neighbours = \s ->
            V.toList (push roomSize s) <> V.toList (pop roomSize s)
        , Dijkstra.find = Dijkstra.FindOne done
        , Dijkstra.start = S.singleton state
        }

main :: IO ()
main = simpleMain $ \input1 ->
    let lines1 = lines input1
        input2 = unlines $ take 3 lines1 ++
            ["  #D#C#B#A#", "  #D#B#A#C#"] ++
            drop 3 lines1 in
    (solve 2 (parseState input1), solve 4 (parseState input2))
