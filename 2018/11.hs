{-# LANGUAGE LambdaCase #-}

import qualified AdventOfCode.Grid   as G
import           AdventOfCode.V2
import           AdventOfCode.V2.Box
import qualified Data.Array          as Arr
import qualified Data.List           as L
import           Data.Ord            (comparing)

hundredsDigit :: Int -> Int
hundredsDigit = (`mod` 10) . (`div` 100)

power :: Int -> G.Pos -> Int
power serial (V2 x y) =
    hundredsDigit (((rackId * y) + serial) * rackId) - 5
  where
    rackId = x + 10

square :: Int -> G.Pos -> [G.Pos]
square n p2 =
    [ p2 .+. V2 x y
    | x <- [0 .. n - 1]
    , y <- [0 .. n - 1]
    ]

largest :: Int -> Int -> (G.Pos, Int)
largest serial n = L.maximumBy (comparing snd) $ do
    pos <- V2 <$> [1 .. 300 - (n - 1)] <*> [1 .. 300 - (n - 1)]
    return (pos, sum $ map (power serial) (square n pos))

showPos :: G.Pos -> String
showPos (V2 x y) = show x ++ "," ++ show y

sumTable
    :: (Num a, Show a)
    => (G.Pos -> a)             -- ^ Individual value
    -> (Int, Int)               -- ^ Width, height
    -> (Box Int -> a)           -- ^ Get sum
sumTable f (w, h) =
    let table = Arr.array ((0, 0), (w, h))
            [((x, y), g x y) | x <- [0 .. w], y <- [0 .. h]]

        g x y
            | x == 0 || y == 0 = 0
            | otherwise        = f (V2 x y) +
                table Arr.! (x, y - 1) + table Arr.! (x - 1, y) -
                table Arr.! (x - 1, y - 1) in

    \(Box (V2 x1 y1) (V2 x2 y2)) ->
        table Arr.! (x2, y2) + table Arr.! (x1 - 1, y1 - 1) -
        table Arr.! (x1 - 1, y2) - table Arr.! (x2, y1 - 1)


main :: IO ()
main = do
    serial <- read <$> getContents
    putStrLn $ showPos $ fst $ largest serial 3

    let boxSum = sumTable (power serial) (300, 300)
    putStrLn $ fst $ L.maximumBy (comparing snd) $ do
        n <- [1 .. 300]
        x <- [1 .. 300 - (n - 1)]
        y <- [1 .. 300 - (n - 1)]
        let pos = V2 x y
            box = Box pos (pos .+. V2 (n - 1) (n - 1))
        return (showPos pos ++ "," ++ show n, boxSum box)
