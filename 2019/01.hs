module Main where

fuel1 :: Int -> Int
fuel1 mass = mass `div` 3 - 2

fuel2 :: Int -> Int
fuel2 = sum . takeWhile (> 0) . drop 1 . iterate fuel1

main :: IO ()
main = do
    masses <- map read . lines <$> getContents
    print $ sum $ map fuel1 masses
    print $ sum $ map fuel2 masses
