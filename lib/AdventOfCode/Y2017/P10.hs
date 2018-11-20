{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.Y2017.P10 where

import qualified AdventOfCode.Y2017.KnotHash as KnotHash
import           Control.Monad.ST            (runST)
import           Data.Proxy                  (Proxy (..))
import qualified Data.Vector.Unboxed         as VU

main :: IO ()
main = do
    input <- getLine
    let lens1 = map read $ words $ map (\c -> if c == ',' then ' ' else c) input
        vec1  = runST (KnotHash.single (Proxy :: Proxy 256) lens1)
    putStrLn $
        "Product of first two numbers: " ++
        show ((vec1 VU.! 0) * (vec1 VU.! 1))

    putStrLn $ "Dense hash: " ++ KnotHash.knotHashHex input
