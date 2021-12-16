module Data.Functor.Fix
    ( Fix (..)
    , cata
    , para
    ) where

newtype Fix f = Fix {unFix :: f (Fix f)}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = f . fmap (\g -> (g, para f g)) . unFix
