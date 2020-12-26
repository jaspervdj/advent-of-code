{-# LANGUAGE DataKinds #-}
import           AdventOfCode.Modulo
import           Data.Semigroup      (stimes)

diagonal :: Int -> Int -> Int
diagonal x y = let left = y + x - 1 in left * (left - 1) `div` 2 + x

newtype Mul = Mul {unMul :: Modulo 33554393} deriving (Show)

instance Semigroup Mul where
    Mul x <> Mul y = Mul $ x * y

code :: Int -> Mul
code n = Mul 20151125 <> stimes (n - 1) (Mul 252533)

main :: IO ()
main = print . unModulo . unMul . code $ diagonal 3083 2978
