{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.Modulo
    ( Modulo
    , mkModulo
    , unModulo
    ) where

import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits (KnownNat, Nat, natVal)

newtype Modulo (m :: Nat) = Modulo {unModulo :: Integer} deriving (Eq)

mkModulo :: forall m. KnownNat m => Integer -> Modulo m
mkModulo x = Modulo $ x `mod` natVal (Proxy :: Proxy m)

instance Show (Modulo m) where
    show (Modulo x) = show x

instance forall m. KnownNat m => Bounded (Modulo m) where
    minBound = 0
    maxBound = Modulo $ natVal (Proxy :: Proxy m) - 1

instance KnownNat m => Num (Modulo m) where
    Modulo x + Modulo y = mkModulo (x + y)
    Modulo x - Modulo y = mkModulo (x - y)
    Modulo x * Modulo y = mkModulo (x * y)
    abs (Modulo x)      = Modulo x
    signum (Modulo _)   = 1
    fromInteger         = mkModulo

