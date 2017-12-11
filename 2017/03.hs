{-# LANGUAGE BangPatterns #-}
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

-- | Coordinates on the 2D grid.
data Pos = Pos !Int !Int deriving (Eq, Ord, Show)

-- | Infinite list containing all the positions in the spiral in order.
positions :: [Pos]
positions = Pos 0 0 : go 1
  where
    go level =
        [Pos level y    | y <- [1 - level .. level]] ++
        [Pos x level    | x <- [level - 1, level - 2 .. -level]] ++
        [Pos (-level) y | y <- [level - 1, level - 2 .. -level]] ++
        [Pos x (-level) | x <- [1 - level .. level]] ++
        go (level + 1)

-- | All neighbours of a position, including diagonals.
neighbours :: Pos -> [Pos]
neighbours (Pos x y) =
    [ Pos (x + dx) (y + dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dx /= 0 || dy /= 0
    ]

-- | We can simply take the nth position in the list.
problem01 :: Int -> Int
problem01 needle =
    let Pos x y = positions !! (needle - 1) in
    abs x + abs y

-- | We fill the grid up dynamically until we have the value we need.
problem02 :: Int -> Int
problem02 needle = go (M.singleton (Pos 0 0) 1) (drop 1 positions)
  where
    go _       []       = 0
    go !values (p : ps) =
        let !v = sum [fromMaybe 0 (M.lookup n values) | n <- neighbours p] in
        if v > needle then v else go (M.insert p v values) ps

main :: IO ()
main = do
    input <- readLn
    putStrLn $ "Steps: " ++ show (problem01 input)
    putStrLn $ "First value written: " ++ show (problem02 input)
