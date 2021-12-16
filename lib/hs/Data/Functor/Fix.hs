{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Fix
    ( Fix (..)
    , cata
    ) where

newtype Fix f = Fix {unFix :: f (Fix f)}

instance (Functor f, Show (f String)) => Show (Fix f) where
    show = cata show

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix
