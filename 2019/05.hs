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
    prog <- NP.hRunParser IO.stdin parseProgram
    print $ last $ evalMachine $ initMachine [1] prog
    print $ last $ evalMachine $ initMachine [5] prog
