{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module AdventOfCode.V2
    ( V2 (..)
    , zero
    , (.+.)
    , (.-.)
    , (.*)
    ) where

data V2 a = V2 {vX :: !a, vY :: !a}
    deriving (Eq, Foldable, Functor, Ord, Show)

zero :: Num a => V2 a
zero = V2 0 0

(.+.) :: Num a => V2 a -> V2 a -> V2 a
V2 x1 y1 .+. V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
infixl 6 .+.

(.-.) :: Num a => V2 a -> V2 a -> V2 a
V2 x1 y1 .-. V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
infixl 6 .-.

(.*) :: Num a => V2 a -> a -> V2 a
V2 x1 y1 .* s = V2 (x1 * s) (y1 * s)
infixl 7 .*
