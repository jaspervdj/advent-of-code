{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module AdventOfCode.BranchAndBound
    ( BranchAndBound (..)
    , branchAndBound
    ) where

import           Data.List (foldl')

class BranchAndBound b where
    type Score b
    score     :: b -> Score b
    potential :: b -> Score b
    next      :: b -> [b]

best :: (BranchAndBound s, Ord (Score s)) => s -> s -> s
best s0 s1 = if score s0 > score s1 then s0 else s1

branchAndBound :: (BranchAndBound s, Ord (Score s)) => s -> s
branchAndBound search0 = go search0 search0
  where
    go best0 current
        | potential current < score best0 = best0
        | otherwise                       =
            foldl' go (best best0 current) (next current)
