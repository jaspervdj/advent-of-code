{-# LANGUAGE BangPatterns #-}
import qualified Data.Map   as M
import qualified AdventOfCode.Grid as G
import           Data.Maybe (fromMaybe)

-- | Infinite list containing all the positions in the spiral in order.
positions :: [G.Pos]
positions = G.Pos 0 0 : go 1
  where
    go level =
        [G.Pos level y    | y <- [1 - level .. level]] ++
        [G.Pos x level    | x <- [level - 1, level - 2 .. -level]] ++
        [G.Pos (-level) y | y <- [level - 1, level - 2 .. -level]] ++
        [G.Pos x (-level) | x <- [1 - level .. level]] ++
        go (level + 1)

-- | We can simply take the nth position in the list.
problem01 :: Int -> Int
problem01 needle =
    let G.Pos x y = positions !! (needle - 1) in
    abs x + abs y

-- | We fill the grid up dynamically until we have the value we need.
problem02 :: Int -> Int
problem02 needle = go (M.singleton (G.Pos 0 0) 1) (drop 1 positions)
  where
    go _       []       = 0
    go !values (p : ps) =
        let next = G.neighbours p ++ G.diagonal p
            !v = sum [fromMaybe 0 (M.lookup n values) | n <- next] in
        if v > needle then v else go (M.insert p v values) ps

main :: IO ()
main = do
    input <- readLn
    putStrLn $ "Steps: " ++ show (problem01 input)
    putStrLn $ "First value written: " ++ show (problem02 input)
