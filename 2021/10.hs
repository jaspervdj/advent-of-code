{-# LANGUAGE LambdaCase #-}
import           AdventOfCode.Main (simpleMain)
import           Data.Foldable     (foldl')
import           Data.List         (sort)

closing :: Char -> Maybe Char
closing = \case
    '<' -> Just '>'
    '[' -> Just ']'
    '(' -> Just ')'
    '{' -> Just '}'
    _   -> Nothing

illegalPoints :: Char -> Int
illegalPoints = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> 0

incompletePoints :: String -> Int
incompletePoints = foldl' (\acc c -> acc * 5 + points c) 0
  where
    points = \case
        ')' -> 1
        ']' -> 2
        '}' -> 3
        '>' -> 4
        _   -> 0

data Validation
    = Illegal    Char
    | Incomplete String
    | Ok
    deriving (Show)

validate :: String -> Validation
validate = go []
  where
    go []    []                       = Ok
    go stack []                       = Incomplete stack
    go stack (x : xs)
        | Just c <- closing x         = go (c : stack) xs
        | s : stack' <- stack, s == x = go stack' xs
        | otherwise                   = Illegal x

main :: IO ()
main = simpleMain $ \input ->
    let validation = map validate $ lines input
        part1 = sum [illegalPoints c | Illegal c <- validation]
        part2 =
            let scores = [incompletePoints s | Incomplete s <- validation] in
            sort scores !! (length scores `div` 2) in
    (part1, part2)
