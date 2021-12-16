module Data.Functor.Fix
    ( Fix (..)
    , cata
    ) where

newtype Fix f = Fix {unFix :: f (Fix f)}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix
