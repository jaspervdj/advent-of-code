module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative
import           Data.Maybe              (fromMaybe)
import qualified Data.Vector             as V

type Binary = V.Vector Bool

binaryToInt :: Binary -> Int
binaryToInt = V.foldl' (\a x -> a * 2 + if x then 1 else 0) 0

parseBinary :: P.Parser Char Binary
parseBinary = fmap V.fromList $
    P.many1 $ (False <$ P.char '0') <|> (True <$ P.char '1')

parseBinaries :: P.Parser Char (V.Vector Binary)
parseBinaries = fmap V.fromList . many $ parseBinary <* P.spaces

mostCommonBit :: Binary -> Maybe Bool
mostCommonBit bin =
    case compare (V.length (V.filter id bin) * 2) (V.length bin) of
        EQ -> Nothing
        GT -> Just True
        LT -> Just False

withCriteria
    :: (Maybe Bool -> Bool -> Bool) -> (V.Vector Binary) -> Maybe Binary
withCriteria criteria = go 0
  where
    go i rows
        | V.length rows == 1 = Just (rows V.! 0)
        | otherwise          =
            let mcb = mostCommonBit $ (V.! i) <$> rows in
            go (i + 1) $ V.filter (criteria mcb . (V.! i)) rows

main :: IO ()
main = pureMain $ \input -> do
    rows <- P.runParser parseBinaries input

    let gammaRate = V.map (fromMaybe False . mostCommonBit) $ do
            i <- V.enumFromN 0 (V.length (rows V.! 0))
            pure $ (V.! i) <$> rows
        epsilonRate = not <$> gammaRate
        powerConsumption = binaryToInt gammaRate * binaryToInt epsilonRate

    let oxygenRating = maybe 0 binaryToInt $
            withCriteria (\mcb b -> b == fromMaybe True mcb) rows
        co2Rating = maybe 0 binaryToInt $
            withCriteria (\mcb b -> b /= fromMaybe True mcb) rows
        lifeSupportRating = oxygenRating * co2Rating

    pure (pure powerConsumption, pure lifeSupportRating)
