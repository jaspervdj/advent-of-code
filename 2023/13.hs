import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import qualified AdventOfCode.Parsing      as Parsing
import           AdventOfCode.V2           (V2 (..))
import           Data.Maybe                (listToMaybe)
import           Data.Monoid               (Sum (..))


col :: G.Grid a -> Int -> [a]
col g x = [g G.! (V2 x y) | y <- [0 .. G.gridHeight g - 1]]

row :: G.Grid a -> Int -> [a]
row g y = [g G.! (V2 x y) | x <- [0 .. G.gridWidth g - 1]]

type Errors = Sum Int

(==?) :: Eq a => a -> a -> Errors
x ==? y = if x == y then 0 else 1

colReflect :: Eq a => G.Grid a -> Int -> Errors
colReflect grid x = mconcat $ do
    (left, right) <- zip lefts rights
    zipWith (==?) (col grid left) (col grid right)
  where
    lefts  = takeWhile (>= 0)               $ iterate pred x
    rights = takeWhile (< G.gridWidth grid) $ iterate succ (x + 1)

rowReflect :: Eq a => G.Grid a -> Int -> Errors
rowReflect grid y = mconcat $ do
    (top, bottom) <- zip tops bottoms
    zipWith (==?) (row grid top) (row grid bottom)
  where
    tops    = takeWhile (>= 0)                $ iterate pred y
    bottoms = takeWhile (< G.gridHeight grid) $ iterate succ (y + 1)

data Reflect = ColReflect Int | RowReflect Int deriving (Show)

reflect :: Eq a => Errors -> G.Grid a -> Maybe Reflect
reflect e g = listToMaybe $
    [ColReflect x | x <- [0 .. G.gridWidth  g - 2], colReflect g x == e] ++
    [RowReflect y | y <- [0 .. G.gridHeight g - 2], rowReflect g y == e]

summarize :: Reflect -> Int
summarize (ColReflect x) = x + 1
summarize (RowReflect y) = (y + 1) * 100

main :: IO ()
main = pureMain $ \str -> do
    grids <- traverse G.fromString $ map unlines $ Parsing.sections str
    let part1 = sum $ map (maybe 0 summarize . reflect 0) grids
        part2 = sum $ map (maybe 0 summarize . reflect 1) grids
    pure (pure part1, pure part2)
