{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
module Main where

import           AdventOfCode.Main
import           AdventOfCode.Modulo

bfLoopSize :: Int -> Integer -> Int
bfLoopSize subject publicKey = go 0 1
  where
    pk = fromInteger publicKey
    sj = fromIntegral subject

    go :: Int -> Modulo 20201227 -> Int
    go !i value
        | value == pk = i
        | otherwise   = go (i + 1) (value * sj)

doLoop :: Int -> Integer -> Integer
doLoop loopSize subject = unModulo $ go 0 1
  where
    sj = fromInteger subject

    go :: Int -> Modulo 20201227 -> Modulo 20201227
    go !i value
        | i >= loopSize = value
        | otherwise     = go (i + 1) (value * sj)

main :: IO ()
main = simpleMain $ \inputstr ->
   let [cardPk, doorPk] = map read $ lines inputstr
       cardLoopSize = bfLoopSize 7 cardPk
       doorLoopSize = bfLoopSize 7 doorPk
       encryptionKey
           | cardLoopSize < doorLoopSize = doLoop cardLoopSize doorPk
           | otherwise                   = doLoop doorLoopSize cardPk in
   (encryptionKey, ())
