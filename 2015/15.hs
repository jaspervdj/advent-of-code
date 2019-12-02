{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Data.Monoid  (Sum (..))
import           GHC.Generics
import qualified System.IO    as IO
import           Text.Read    (readMaybe)

--------------------------------------------------------------------------------
-- A different kind of boilerplate.

class GSemigroup f where
    genericAppend :: f k -> f k -> f k

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
    genericAppend (x1 :*: y1) (x2 :*: y2) =
        genericAppend x1 x2 :*: genericAppend y1 y2

instance GSemigroup f => GSemigroup (M1 i c f) where
    genericAppend (M1 x) (M1 y) = M1 (genericAppend x y)

instance Semigroup c => GSemigroup (K1 i c) where
    genericAppend (K1 x) (K1 y) = K1 (x <> y)

newtype GenericSemigroup a = GenericSemigroup a

instance (Generic a, GSemigroup (Rep a)) => Semigroup (GenericSemigroup a) where
    GenericSemigroup x <> GenericSemigroup y =
        GenericSemigroup $ to (genericAppend (from x) (from y))

class GSemigroup f => GMonoid f where
    genericEmpty :: f k

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
    genericEmpty = (genericEmpty :*: genericEmpty)

instance GMonoid f => GMonoid (M1 i c f) where
    genericEmpty = M1 genericEmpty

instance Monoid c => GMonoid (K1 i c) where
    genericEmpty = K1 mempty

newtype GenericMonoid a = GenericMonoid a

instance (Generic a, GSemigroup (Rep a)) => Semigroup (GenericMonoid a) where
    GenericMonoid x <> GenericMonoid y =
        GenericMonoid $ to (genericAppend (from x) (from y))

instance (Generic a, GMonoid (Rep a)) => Monoid (GenericMonoid a) where
    mempty = GenericMonoid (to genericEmpty)

--------------------------------------------------------------------------------
-- Actual solution (with the monoid instance we derived).

data Properties a = Properties
    { capacity   :: a
    , durability :: a
    , flavor     :: a
    , texture    :: a
    , calories   :: a
    }
    deriving (Eq, Foldable, Functor, Generic, Show)
    deriving Semigroup via (GenericSemigroup (Properties a))
    deriving Monoid    via (GenericMonoid    (Properties a))

readIngredients :: IO.Handle -> IO [Properties Int]
readIngredients h =
    IO.hGetContents h >>= mapM readIngredient . lines
  where
    readIngredient line = case map readMaybe . words . filter (/= ',') $ line of
        [ _ , _, Just capacity, _, Just durability, _, Just flavor, _
            , Just texture, _, Just calories] -> pure Properties {..}
        _ -> fail $ "Can't parse: " ++ show line

parts :: Int -> [a] -> [[(Int, a)]]
parts 0 []       = [[]]
parts _ []       = []
parts n (x : xs) =
    [ (m, x) : ys
    | m  <- [0 .. n]
    , ys <- parts (n - m) xs
    ]

mix :: [(Int, Properties Int)] -> Properties Int
mix = fmap getSum . foldMap (\(n, prop) -> Sum . (* n) <$> prop)

score :: Properties Int -> Int
score = product . (\prop -> prop {calories = 1}) . fmap (max 0)

main :: IO ()
main = do
    ingredients <- readIngredients IO.stdin
    print . maximum . map (score . mix) $ parts 100 ingredients
    print . maximum . map score . filter ((== 500) . calories) .  map mix $
        parts 100 ingredients
