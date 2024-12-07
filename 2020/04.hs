module Main where

import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))
import           Control.Monad           (guard)
import           Data.Char               (isDigit, isSpace)
import           Data.Foldable           (toList)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (All (..))
import qualified System.IO               as IO
import           Text.Read               (readMaybe)

type Passport = Map.Map String String

parsePassports :: P.Parser Char [Passport]
parsePassports = P.sepBy
    (Map.fromList . toList <$> P.sepBy1 entry sep)
    (P.string "\n\n")
  where
    sep    = P.char ' ' <|> P.char '\n'
    entry  = (,) <$> (key <* P.char ':') <*> value
    key    = toList <$> P.many1 P.alpha
    value  = fmap toList $ P.many1 $ P.satisfy "value char" (not . isSpace)

type Validator = Map.Map String (String -> Bool)

part1 :: Validator
part1 = const True <$ part2

part2 :: Validator
part2 = Map.fromList
    [ ("byr", year 1920 2002)
    , ("iyr", year 2010 2020)
    , ("eyr", year 2020 2030)
    , ("hgt", parse height)
    , ("hcl", parse hair)
    , ("ecl", eyes)
    , ("pid", pid)
    ]
  where
    year :: Int -> Int -> String -> Bool
    year lo hi = \str -> fromMaybe False $ do
        guard $ length str == 4 && all isDigit str
        between lo hi <$> readMaybe str

    height =
        (between 150 193 <$> P.decimal <* P.string "cm") <|>
        (between 59 76 <$> P.decimal <* P.string "in")

    pid str = length str == 9 && all isDigit str
    hair    = True <$ (P.char '#' *> hex *> hex *> hex *> hex *> hex *> hex)
    eyes    = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    between = \lo hi x -> x >= lo && x <= hi
    hex     = P.satisfy "hex" $ \c -> isDigit c || c >= 'a' && c <= 'f'
    parse p = either (const False) id . P.runParser p


valid :: Passport -> Validator -> Bool
valid passport v = getAll $ Map.foldMapWithKey
    (\k f -> All $ Just True == fmap f (Map.lookup k passport))
    v

main :: IO ()
main = do
    passports <- P.hRunParser IO.stdin parsePassports
    print . length $ filter (`valid` part1) passports
    print . length $ filter (`valid` part2) passports
