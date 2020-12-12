{-# LANGUAGE LambdaCase #-}
module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified Data.List               as L
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import qualified System.IO               as IO

tupled :: [a] -> [(a, a)]
tupled (x : y : zs) = (x, y) : tupled zs
tupled _            = []

data State = State !(G.Grid Int) !G.Dir !G.Pos deriving (Show)

runRobot :: Program -> State -> State
runRobot prog state0 =
    let states  = L.scanl' step state0 (tupled outputs)
        inputs  = map camera states
        outputs = evalMachine (initMachine inputs prog) in
    last states
  where
    step :: State -> (Int, Int) -> State
    step (State grid dir pos) (colorSignal, dirSignal) =
        let dir'  = if dirSignal == 0 then G.turnLeft dir else G.turnRight dir
            grid' = Map.insert pos colorSignal grid in
        State grid' dir' (G.move 1 dir' pos)

    camera :: State -> Int
    camera (State g _ p) = fromMaybe 0 (Map.lookup p g)

main :: IO ()
main = do
    program <- NP.hRunParser IO.stdin parseProgram
    let state1          = State Map.empty G.U G.origin
        state2          = State (Map.singleton G.origin 1) G.U G.origin
        State grid1 _ _ = runRobot program state1
        State grid2 _ _ = runRobot program state2
    print . length $ Map.toList grid1
    G.printGrid IO.stdout $ fmap (\c -> if c == 0 then ' ' else '#') grid2
