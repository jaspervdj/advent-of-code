{-# LANGUAGE DeriveFoldable #-}
module AdventOfCode.V2.Box
    ( Box (..)
    , fromV2
    , width
    , height
    , area
    , inside
    ) where

import           AdventOfCode.V2

data Box a = Box
    { bTopLeft     :: !(V2 a)
    , bBottomRight :: !(V2 a)
    } deriving (Eq, Foldable, Show)

instance Ord a => Semigroup (Box a) where
    Box (V2 lx1 ty1) (V2 rx1 by1) <> Box (V2 lx2 ty2) (V2 rx2 by2) =
        Box (V2 (min lx1 lx2) (min ty1 ty2)) (V2 (max rx1 rx2) (max by1 by2))

fromV2 :: V2 a -> Box a
fromV2 v2 = Box v2 v2

width, height :: Num a => Box a -> a
width  (Box (V2 lx _)  (V2 rx _))  = rx - lx + 1
height (Box (V2 _  ty) (V2 _  by)) = by - ty + 1

area :: Num a => Box a -> a
area b = width b * height b

inside :: Ord a => V2 a -> Box a -> Bool
inside (V2 x y) (Box (V2 x0 y0) (V2 x1 y1)) =
    x >= x0 && x <= x1 && y >= y0 && y <= y1
