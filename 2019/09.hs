module Main where

import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified System.IO               as IO

main :: IO ()
main = do
    program <- NP.hRunParser IO.stdin parseProgram
    print $ head $ evalMachine $ initMachine [1] program
    print $ head $ evalMachine $ initMachine [2] program
