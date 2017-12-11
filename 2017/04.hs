import           Data.List (sort)
import qualified Data.Set  as S

valid :: Ord a => [a] -> Bool
valid = go S.empty
  where
    go _   [] = True
    go acc (x : xs)
        | x `S.member` acc = False
        | otherwise        = go (S.insert x acc) xs

numValid :: Ord a => [[a]] -> Int
numValid = length . filter valid

newtype Anagram = Anagram String deriving (Eq, Ord, Show)

mkAnagram :: String -> Anagram
mkAnagram = Anagram . sort

main :: IO ()
main = do
    passphrases <- map words . lines <$> getContents
    putStrLn $ "Valid phrases: " ++ show (numValid passphrases)
    let anagrams = map (map mkAnagram) passphrases
    putStrLn $ "Valid anagram phrases: " ++ show (numValid anagrams)
