-- | Some relatively simple code to find repeating loops in an iteration.
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
module AdventOfCode.Loop
    ( Loop (..)
    , findLoop
    , equivalent
    ) where

import qualified Data.Map as M

-- | A detected loop.
--
-- Calling the step function on 'lLast' should produce something that has the
-- same key as 'lFirst'.
data Loop a = Loop
    { lStart  :: !Int  -- ^ First index of the repeated pattern.
    , lFirst  :: !a    -- ^ State at the first index of the first cycle.
    , lLength :: !Int  -- ^ Length of the loop.
    , lSecond :: !a    -- ^ State at the first index of the second cycle.
    } deriving (Functor, Show)

findLoop
    :: Ord k
    => (a -> k)        -- ^ Conversion to a key that has some equality measure
    -> (a -> a)        -- ^ Step function
    -> a               -- ^ Initial state
    -> Maybe (Loop a)  -- ^ Resulting loop
findLoop toKey f = go M.empty 0
  where
    go memory !i !x = case M.lookup xk memory of
        Just (j, y) -> Just $ Loop j y (i - j) x
        Nothing     ->
            go (M.insert xk (i, x) memory) (i + 1) (f x)
      where
        xk = toKey x

-- | Find the equivalent (smaller) index to the given (large) index.
equivalent :: Loop a -> Int -> Int
equivalent l large = lStart l + ((large - lStart l) `mod` lLength l)
