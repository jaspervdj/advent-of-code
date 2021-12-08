module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     (many)
import           Control.Monad           (forM, guard)
import           Data.Char               (toUpper)
import           Data.Foldable           (foldl')
import           Data.Foldable.Extra     (minimaBy)
import qualified Data.Map                as M
import           Data.Ord                (comparing)
import qualified Data.Set                as S
import           Text.Read               (readMaybe)

data Segment = A | B | C | D | E | F | G
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

parseSegment :: P.Parser Char Segment
parseSegment = P.satisfyMaybe "abcdefg" (\c -> readMaybe [toUpper c])

type Pattern = S.Set Segment

parsePattern :: P.Parser Char Pattern
parsePattern = S.fromList <$> P.many1 parseSegment

-- | Original configuration
display :: [(Pattern, Int)]
display =
    [ (S.fromList [A, B, C, E, F, G],    0)
    , (S.fromList [C, F],                1)
    , (S.fromList [A, C, D, E, G],       2)
    , (S.fromList [A, C, D, F, G],       3)
    , (S.fromList [B, C, D, F],          4)
    , (S.fromList [A, B, D, F, G],       5)
    , (S.fromList [A, B, D, E, F, G],    6)
    , (S.fromList [A, C, F],             7)
    , (S.fromList [A, B, C, D, E, F, G], 8)
    , (S.fromList [A, B, C, D, F, G],    9)
    ]

data Input = Input [Pattern] [Pattern] deriving (Show)

parseInputs :: P.Parser Char [Input]
parseInputs = many $ Input
    <$> (P.many1 (tok parsePattern) <* tok (P.char '|'))
    <*> (P.many1 (tok parsePattern) <* P.spaces)
  where
    tok p = p <* P.horizontalSpaces

type Mapping = M.Map Segment Segment

solve :: [Pattern] -> Either String Mapping
solve patterns = case go M.empty of
    [m] | all (`M.member` m) [minBound .. maxBound] -> Right m
    [_]                                             -> Left "Incomplete"
    []                                              -> Left "Unsolvable"
    _                                               -> Left "Ambiguous"
  where
    go :: M.Map Segment Segment -> [M.Map Segment Segment]
    go known = case minimaBy (comparing (length . snd)) branches of
        []          -> pure known
        (s, ts) : _ -> foldMap (\t -> go (M.insert t s known)) ts
      where
        branches = do
            s <- [minBound .. maxBound]
            guard $ s `notElem` known
            pure (s, solveFor s)

        solveFor s = do
            t <- [minBound .. maxBound]
            guard . not $ t `M.member` known
            guard $ countBySize (map fst display) s == countBySize patterns t
            pure t

    -- Count how many times a segment occurs in all patterns, grouped by size.
    -- These counts must match for equivalent patterns from the display and
    -- the input.
    countBySize :: [Pattern] -> Segment -> M.Map Int Int
    countBySize pats s =
        M.fromListWith (+) [(S.size p, 1 :: Int) | p <- pats, s `S.member` p]

decodeDigit :: Mapping -> Pattern -> Maybe Int
decodeDigit mapping pat = do
    pat' <- fmap S.fromList . mapM (`M.lookup` mapping) $ S.toList pat
    lookup pat' display

decodeDigits :: Mapping -> [Pattern] -> Maybe Int
decodeDigits mapping pats =
    foldl' (\acc x -> acc * 10 + x) 0 <$> mapM (decodeDigit mapping) pats

main :: IO ()
main = pureMain $ \input -> do
    samples <- P.runParser parseInputs input

    let part1 = length $ do
            Input _ outs <- samples
            out <- outs
            guard $ length [p | (p, _) <- display, S.size p == S.size out] == 1
            pure out

        part2 = fmap sum . forM samples $ \(Input pats outs) -> do
            mapping <- solve pats
            maybe (Left "Decoding failed") Right $ decodeDigits mapping outs

    pure (pure part1, part2)
