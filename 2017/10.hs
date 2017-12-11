{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified AdventOfCode.KnotHash as KnotHash
import           Control.Monad.ST      (runST)
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector.Unboxed   as VU

main :: IO ()
main = do
    input <- getLine
    let lens1 = map read $ words $ map (\c -> if c == ',' then ' ' else c) input
        vec1  = runST (KnotHash.single (Proxy :: Proxy 256) lens1)
    putStrLn $
        "Product of first two numbers: " ++
        show ((vec1 VU.! 0) * (vec1 VU.! 1))

    putStrLn $ "Dense hash: " ++ KnotHash.knotHashHex input
