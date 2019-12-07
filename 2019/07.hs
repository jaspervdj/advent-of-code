{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.NanoTest
import           Data.List               (foldl', permutations)
import qualified System.IO               as IO

linear :: Program -> [Int] -> Int
linear prog =
    foldl' (\acc phase -> last $ evalMachine $ initMachine [phase, acc] prog) 0

looped :: Program -> [Int] -> Int
looped prog phases =
    let outputs = do
            (idx, phase) <- zip [0 ..] phases
            let prev  = (idx - 1) `mod` length outputs
                input = [phase] ++ [0 | idx == 0] ++ outputs !! prev
            pure $ evalMachine $ initMachine input prog in
    last $ last outputs

main :: IO ()
main = do
    program <- NP.hRunParser IO.stdin parseProgram
    print $ maximum $ map (linear program) $ permutations [0 .. 4]
    print $ maximum $ map (looped program) $ permutations [5 .. 9]

test :: IO ()
test = do
    looped
        (makeProgram
            [ 3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26
            , 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
            ])
        [9, 8, 7, 6, 5] @?= 139629729
    looped
        (makeProgram
            [ 3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005
            , 55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55
            , 1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6
            , 99,0,0,0,0,10
            ])
        [9, 7, 8, 5, 6] @?= 18216
