type Spreadsheet = [[Int]]

readSpreadsheet :: IO Spreadsheet
readSpreadsheet = map (map read . words) . lines <$> getContents

checksum :: Spreadsheet -> Int
checksum spreadsheet = sum [maximum row - minimum row | row <- spreadsheet]

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

evenly :: Spreadsheet -> Int
evenly spreadsheet = sum
    [ x `div` y
    | row       <- spreadsheet
    , (x, row') <- select row
    , (y, _)    <- select row'
    , x >= y
    , x `mod` y == 0
    ]

main :: IO ()
main = do
    spreadsheet <- readSpreadsheet
    putStrLn $ "Checksum: " ++ show (checksum spreadsheet)
    putStrLn $ "Evenly divisible: " ++ show (evenly spreadsheet)
