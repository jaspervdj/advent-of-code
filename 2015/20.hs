module Main where

divs :: Int -> [Int]
divs n = go 1
  where
    go x
        | x * x == n = [x]
        | x * x >  n = []
        | otherwise  =
            let (y, r) = n `divMod` x in
            (if r == 0 then [x, y] else []) ++ go (x + 1)

main :: IO ()
main = do
    -- This code is very slow and quite ugly, but at least it's extremely terse
    -- so it doesn't offend for too long.
    n <- read <$> getContents
    print $ head [h | h <- [1 ..], sum (divs h) * 10 >= n]
    print $ head [h | h <- [1 ..], sum [g | g <- divs h, h <= g * 50] * 11 >= n]
