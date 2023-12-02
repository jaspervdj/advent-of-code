import AdventOfCode.Main

import Data.Char (isDigit)
import Data.List (isPrefixOf)

digits1 :: String -> [Char]
digits1 = filter isDigit

digits2 :: String -> [Char]
digits2 [] = []
digits2 str@(h : t)
    | isDigit h                = h   : digits2 t
    | "one"   `isPrefixOf` str = '1' : digits2 t
    | "two"   `isPrefixOf` str = '2' : digits2 t
    | "three" `isPrefixOf` str = '3' : digits2 t
    | "four"  `isPrefixOf` str = '4' : digits2 t
    | "five"  `isPrefixOf` str = '5' : digits2 t
    | "six"   `isPrefixOf` str = '6' : digits2 t
    | "seven" `isPrefixOf` str = '7' : digits2 t
    | "eight" `isPrefixOf` str = '8' : digits2 t
    | "nine"  `isPrefixOf` str = '9' : digits2 t
    | otherwise                =       digits2 t

main :: IO ()
main = simpleMain $ \input ->
    let calibrate l = read [head l, last l] :: Int
        solve d     = sum $ map (\l -> calibrate $ d l) $ lines input in
    (solve digits1, solve digits2)
