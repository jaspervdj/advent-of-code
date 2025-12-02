import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP

parseProductIDs :: NP.Parser Char [(Int, Int)]
parseProductIDs = NP.sepBy parseRange (NP.char ',' *> NP.spaces)
  where
    parseRange = (,)
        <$> NP.decimal
        <*  NP.char '-'
        <*> NP.decimal

-- Invalid IDs of length 2*n are basically (k * 10^n) + k.
--
-- This means that the first invalid ID of a specific length will always be
-- something like 100..100.., and the last invalid ID of a specific length
-- will always be 999..999...
nextInvalidID1 :: Integral a => a -> a
nextInvalidID1 prev
    | digits `mod` 2 == 1 = 10 * tens * tens + tens  -- Skip to even digits
    | front + 1 >= tens   = 10 * tens * tens + tens  -- Boundary, skip
    | back < front        = front * tens + front
    | otherwise           = (front + 1) * tens + (front + 1)
  where
    digits        = numDigits prev  -- Avoid recomputing?
    half          = digits `div` 2
    tens          = 10 ^ half
    (front, back) = prev `divMod` tens

numDigits :: Integral a => a -> Int
numDigits = go 1
  where
    go !acc n = if n <= 9 then acc else go (acc + 1) (n `div` 10)

isInvalidID2 :: Show a => Eq a => [a] -> Bool
isInvalidID2 [] = False
isInvalidID2 (x0 : xs0) = go 1 [x0] (length xs0) xs0
  where
    go patLen pat strLen str
        | patLen > strLen                   = False
        | remainder == 0 && str == repeated = True
        | otherwise                         = case str of
            x : xs -> go (patLen + 1) (pat ++ [x]) (strLen - 1) xs
            _      -> False
      where
        (reps, remainder) = strLen `divMod` patLen
        repeated          = concat $ replicate reps pat

main :: IO ()
main = pureMain $ \str -> do
    productIDs <- NP.runParser parseProductIDs str
    let part1 = sum $ do
            (lo, hi) <- productIDs
            takeWhile (<= hi) $ drop 1 $ iterate nextInvalidID1 (lo - 1)
    let part2 = sum $ do
            (lo, hi) <- productIDs
            filter (isInvalidID2 . show) [lo .. hi]
    pure (pure part1, pure part2)
