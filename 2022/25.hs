{-# LANGUAGE BangPatterns #-}
import           AdventOfCode.Main
import           Control.Monad     (guard)

data Digit
    = DoubleMinus
    | Minus
    | Zero
    | One
    | Two
    deriving (Bounded, Enum, Eq)

parseDigit :: Char -> Either String Digit
parseDigit c = case c of
    '2' -> Right Two
    '1' -> Right One
    '0' -> Right Zero
    '-' -> Right Minus
    '=' -> Right DoubleMinus
    _   -> Left $ "unexpected character: " ++ show c

digitToChar :: Digit -> Char
digitToChar d = case d of
    Two         -> '2'
    One         -> '1'
    Zero        -> '0'
    Minus       -> '-'
    DoubleMinus -> '='

digitToInt :: Digit -> Int
digitToInt d = case d of
    Two         -> 2
    One         -> 1
    Zero        -> 0
    Minus       -> -1
    DoubleMinus -> -2

newtype Snafu = Snafu [Digit]

instance Show Snafu where
    show (Snafu snafu) = map digitToChar snafu

parseSnafu :: String -> Either String Snafu
parseSnafu = fmap Snafu . traverse parseDigit

snafuToInt :: Snafu -> Int
snafuToInt (Snafu snafu) = go 0 1 $ reverse snafu
  where
    go !acc _      []       = acc
    go !acc !place (x : xs) = go (acc + digitToInt x * place) (place * 5) xs

intToSnafu :: Int -> Snafu
intToSnafu n = Snafu . dropWhile (== Zero) . search [] $
    head [s | s <-  [0 ..] , snafuToInt (Snafu (replicate s maxBound)) >= n]
  where
    search leading remaining | remaining <= 0 = leading
    search leading remaining = do
        digit <- [minBound .. maxBound]
        let lo = Snafu $ leading ++ digit : replicate (remaining - 1) minBound
            hi = Snafu $ leading ++ digit : replicate (remaining - 1) maxBound
        guard $ snafuToInt lo <= n && snafuToInt hi >= n
        search (leading ++ [digit]) (remaining - 1)

main :: IO ()
main = pureMain $ \input -> do
    snafus <- traverse parseSnafu $ lines input
    let total = show . intToSnafu . sum $ map snafuToInt snafus
    pure (pure total, pure "only one part today")
