{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE Rank2Types     #-}
module AdventOfCode.V3
    ( V3 (..)
    , zero
    , (.+.)
    , (.-.)
    , (.*)
    , sum
    , zipWith
    , zipWith3
    , mapWithIndex
    , fromV2
    , manhattan
    ) where

import qualified AdventOfCode.V2 as V2
import qualified Data.Foldable   as F
import           Prelude         hiding (sum, zipWith, zipWith3)

data V3 a = V3 {v3X :: !a, v3Y :: !a, v3Z :: !a}
    deriving (Eq, Foldable, Functor, Ord, Show)

zero :: Num a => V3 a
zero = V3 0 0 0

(.+.) :: Num a => V3 a -> V3 a -> V3 a
(.+.) = zipWith (+)
infixl 6 .+.

(.-.) :: Num a => V3 a -> V3 a -> V3 a
(.-.) = zipWith (-)
infixl 6 .-.

(.*) :: Num a => V3 a -> a -> V3 a
V3 x1 y1 z1 .* s = V3 (x1 * s) (y1 * s) (z1 * s)
infixl 7 .*

sum :: (Foldable f, Num a) => f (V3 a) -> V3 a
sum = F.foldl' (.+.) zero

zipWith :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
zipWith f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)
{-# INLINE zipWith #-}

zipWith3 :: (a -> b -> c -> d) -> V3 a -> V3 b -> V3 c -> V3 d
zipWith3 f (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) =
    V3 (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)
{-# INLINE zipWith3 #-}

mapWithIndex :: ((forall e. V3 e -> e) -> a -> b) -> V3 a -> V3 b
mapWithIndex f (V3 x y z) = V3 (f v3X x) (f v3Y y) (f v3Z z)
{-# INLINE mapWithIndex #-}

-- | Project a V2 vector on the Z=0 plane.
fromV2 :: Num a => V2.V2 a -> V3 a
fromV2 v2 = V3 (V2.v2X v2) (V2.v2Y v2) 0

manhattan :: Num a => V3 a -> V3 a -> a
manhattan (V3 x1 y1 z1) (V3 x2 y2 z2) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
