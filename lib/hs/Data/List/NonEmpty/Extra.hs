module Data.List.NonEmpty.Extra
    ( module Data.List.NonEmpty
    , foldl1'
    ) where

import qualified Data.List          as L
import           Data.List.NonEmpty

foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (x :| xs) = L.foldl' f x xs
