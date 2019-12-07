{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified System.IO               as IO

main :: IO ()
main = do
    mem <- NP.hRunParser IO.stdin parseMemory
    print $ head $ machineOutputs $ fst $ runMachine $ Machine [1] [] 0 mem
    print $ head $ machineOutputs $ fst $ runMachine $ Machine [5] [] 0 mem
