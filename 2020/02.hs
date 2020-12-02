import qualified AdventOfCode.NanoParser as NP
import qualified System.IO               as IO

data Entry a = Entry (Policy a) [a] deriving (Show)
data Policy a = Policy Int Int a deriving (Show)

parseEntries :: NP.Parser Char [Entry Char]
parseEntries = NP.sepBy entry (NP.char '\n')
  where
    entry = Entry <$> policy <* NP.char ':' <* NP.spaces <*> NP.many1 NP.alpha
    policy = Policy <$>
        NP.decimal <* NP.char '-' <*> NP.decimal <* NP.spaces <*> NP.alpha

check1 :: Eq a => Entry a -> Bool
check1 (Entry (Policy lo hi c) str) =
  let count = length $ filter (== c) str in
  count >= lo && count <= hi

check2 :: Eq a => Entry a -> Bool
check2 (Entry (Policy lo hi c) str) =
  let indices = [i | (i, c') <- zip [1 ..] str, i == lo || i == hi, c == c'] in
  indices == [lo] || indices == [hi]

main :: IO ()
main = do
    entries <- NP.hRunParser IO.stdin parseEntries
    print . length $ filter check1 entries
    print . length $ filter check2 entries
