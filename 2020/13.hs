module Main where

import           AdventOfCode.Main
import           Text.Read         (readMaybe)

-- | An equation of the form `n * 3 = x + 2`.
data Equation = Equation Int Int

instance Show Equation where
    show (Equation n k) = "n * " ++ show n ++ " = x + " ++ show k

-- | The key insight for me was that we can combine two such equations.
instance Semigroup Equation where
    Equation n k <> Equation m l = Equation (n * m) (solver k l)
      where
        solver x y = case compare x y of
            EQ -> x
            LT -> solver (x + n * max 1 ((y - x) `div` n)) y
            GT -> solver x (y + m * max 1 ((x - y) `div` m))

-- | Because every integer satisfies `n * 1 = x + 0`, we have an identity
-- element as well.
instance Monoid Equation where
    mempty = Equation 1 0

main :: IO ()
main = simpleMain $ \input ->
    -- Parse input.
    let [line0, line1] = lines $ map (\c -> if c == ',' then ' ' else c) input
        timestamp = read line0 :: Int
        busses = map readMaybe $ words line1 :: [Maybe Int] in
    -- Part 1.
    ( uncurry (*) . minimum $ do
        Just b <- busses
        let r = timestamp `mod` b
        pure (if r == 0 then 0 else b - r, b)
    -- Part 2.
    , case mconcat [Equation b t | (t, Just b) <- zip [0 ..] busses] of
          Equation n k -> n - k
    )
