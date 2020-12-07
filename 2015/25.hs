{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Proxy     (Proxy (..))
import           Data.Semigroup (stimes)
import           GHC.TypeLits   (KnownNat, Nat, natVal)

diagonal :: Int -> Int -> Int
diagonal x y = let left = y + x - 1 in left * (left - 1) `div` 2 + x

newtype Modulo (n :: Nat) = Modulo {unModulo :: Integer} deriving (Show)

instance forall n. KnownNat n => Semigroup (Modulo n) where
    Modulo x <> Modulo y = Modulo . rem (x * y) $ natVal (Proxy :: Proxy n)

code :: Int -> Modulo 33554393
code n = Modulo 20151125 <> stimes (n - 1) (Modulo 252533)

main :: IO ()
main = print . unModulo . code $ diagonal 3083 2978
