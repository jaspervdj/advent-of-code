module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     (many)
import qualified Data.Map                as M

data Line = Line (V2 Int) (V2 Int)

input :: P.Parser Char [Line]
input = many $ Line <$> (v2 <* P.string "->" <* P.spaces) <*> v2
  where
    v2 = V2 <$> (P.decimal <* P.char ',') <*> (P.decimal <* P.spaces)

oblique :: Line -> Bool
oblique (Line (V2 x0 y0) (V2 x1 y1)) = x0 /= x1 && y0 /= y1

points :: Line -> [V2 Int]
points (Line (V2 x0 y0) (V2 x1 y1))
    | x0 == x1  = [V2 x0 y | y <- range y0 y1]
    | y0 == y1  = [V2 x y0 | x <- range x0 x1]
    | otherwise = zipWith V2 (range x0 x1) (range y0 y1)
  where
    range n k = if n <= k then [n .. k] else [n, n - 1 .. k]

overlaps :: [Line] -> Int
overlaps ls = M.size . M.filter (>= 2) $
    M.fromListWith (+) [(p, 1 :: Int) | l <- ls, p <- points l]

main :: IO ()
main = pureMain $ \str -> do
    ls <- P.runParser input str
    let straight = filter (not . oblique) ls
    pure (pure (overlaps straight), pure (overlaps ls))
