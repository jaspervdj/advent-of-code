module Main where

import qualified Data.Vector as V
import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.Char (isUpper)
import Data.List (transpose)

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

pop :: State -> V.Vector State
pop (hw, rms) = do
    (i, (x, correct, stack)) <- V.indexed $ V.zip3 entrances desired rms
    guard $ any (/= correct) stack
    guard $ not (null stack)
    x' <- V.fromList $ filter (`notElem` entrances) $ free pred x <> free succ x
    pure (hw V.// [(x', Just (head stack))], rms V.// [(i, tail stack)])
  where
    free f x
        | x < 0 || x >= V.length hw = []
        | isJust (hw V.! x)         = []
        | otherwise                 = x : free f (f x)
