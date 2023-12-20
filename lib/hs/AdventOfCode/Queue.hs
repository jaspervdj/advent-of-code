module AdventOfCode.Queue
    ( Queue
    , empty
    , singleton
    , fromList
    , push
    , pop
    ) where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton x = Queue [x] []

fromList :: [a] -> Queue a
fromList front = Queue front []

push :: a -> Queue a -> Queue a
push x (Queue front back) = Queue front (x : back)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []       []  ) = Nothing
pop (Queue (x : xs) back) = Just (x, Queue xs back)
pop (Queue []       back) = pop (Queue (reverse back) [])
