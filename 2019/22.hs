{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           AdventOfCode.Main
import           Data.Proxy        (Proxy (..))
import           Data.Semigroup    (stimes)
import           GHC.TypeLits      (KnownNat, Nat, natVal)
import           Text.Read         (readMaybe)

data Technique
    = DealIntoNewStack
    | Cut !Integer
    | DealWithIncrement !Integer
    deriving (Show)

parseTechnique :: String -> Either String Technique
parseTechnique line = case words line of
    ["deal", "into", "new", "stack"] -> pure DealIntoNewStack
    ["cut", s] | Just n <- readMaybe s -> pure $ Cut n
    ["deal", "with", "increment", s] | Just n <- readMaybe s -> pure $
        DealWithIncrement n
    _ -> Left $ "Could not parse shuffle: " ++ line

newtype Modulo (m :: Nat) = Modulo {unModulo :: Integer}

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
    signum (Modulo _)   = fromInteger 1
    fromInteger         = mkModulo

data Shuffle (m :: Nat) = Shuffle !(Modulo m) !(Modulo m)

instance KnownNat m => Semigroup (Shuffle m) where
    Shuffle a0 b0 <> Shuffle a1 b1 = Shuffle (a0 * a1) (a1 * b0 + b1)

instance KnownNat m => Monoid (Shuffle m) where
    mempty = Shuffle 1 0

techniqueToShuffle :: KnownNat m => Technique -> Shuffle m
techniqueToShuffle = \case
    Cut n               -> Shuffle 1 (negate (mkModulo n))
    DealWithIncrement n -> Shuffle (mkModulo n) 0
    DealIntoNewStack    -> Shuffle maxBound maxBound

invert :: forall m. KnownNat m => Shuffle m -> Shuffle m
invert (Shuffle a b) =
    let a' = a ^ (unModulo (maxBound :: Modulo m) - 1) in
    Shuffle a' (-a' * b)

unShuffle :: forall m. KnownNat m => Shuffle m -> Modulo m -> Modulo m
unShuffle (Shuffle a b) k = k * a + b

main :: IO ()
main = pureMain $ \input -> do
    techs <- traverse parseTechnique $ lines input
    let part1 :: Modulo 10007
        part1 = unShuffle (foldMap techniqueToShuffle techs) 2019
        part2 :: Modulo 119315717514047
        part2 = ($ 2020) . unShuffle . invert .
            stimes (101741582076661 :: Integer) $
            foldMap techniqueToShuffle techs
    pure (pure $ unModulo part1, pure $ unModulo part2)
