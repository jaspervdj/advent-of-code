import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Bifunctor          (first)
import           Data.Foldable           (toList)
import           Data.List               (group, maximumBy, sort, sortOn)
import           Data.Ord                (comparing)

type Hand = [Card]
data Card
    = CJoker | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA
    deriving (Bounded, Enum, Eq, Ord, Show)

parseCard :: NP.Parser Char Card
parseCard =
    (C2 <$ NP.char '2') <|>
    (C3 <$ NP.char '3') <|>
    (C4 <$ NP.char '4') <|>
    (C5 <$ NP.char '5') <|>
    (C6 <$ NP.char '6') <|>
    (C7 <$ NP.char '7') <|>
    (C8 <$ NP.char '8') <|>
    (C9 <$ NP.char '9') <|>
    (CT <$ NP.char 'T') <|>
    (CJ <$ NP.char 'J') <|>
    (CQ <$ NP.char 'Q') <|>
    (CK <$ NP.char 'K') <|>
    (CA <$ NP.char 'A')

parseInput :: NP.Parser Char [(Hand, Int)]
parseInput = fmap toList $ NP.many1 $ (,)
    <$> (toList <$> NP.many1 parseCard <* NP.spaces)
    <*> (NP.decimal <* NP.spaces)

data Score
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving (Eq, Ord, Show)

judge :: Hand -> Score
judge cards
    | any (== 5) groups                      = FiveOfAKind
    | any (== 4) groups                      = FourOfAKind
    | any (== 3) groups && any (== 2) groups = FullHouse
    | any (== 3) groups                      = ThreeOfAKind
    | length groups == 3                     = TwoPair
    | any (== 2) groups                      = OnePair
    | otherwise                              = HighCard
  where
    groups = map length . group $ sort cards

genJokers :: Hand -> [Hand]
genJokers []            = [[]]
genJokers (CJoker : cs) = (:) <$> [minBound .. maxBound] <*> genJokers cs
genJokers (c      : cs) = (c :) <$> genJokers cs

main :: IO ()
main = pureMain $ \instr -> do
    input <- NP.runParser parseInput instr
    let solve f = sum $ do
            (rank, (_, bid)) <- zip [1 ..] $ sortOn fst $ map (first f) input
            pure (rank * bid)

        part1 hand = (judge hand, hand)
        part2 hand =
            let jokers = map (\c -> if c == CJ then CJoker else c) hand in
            (judge $ maximumBy (comparing part1) (genJokers jokers), jokers)

    pure (pure (solve part1), pure (solve part2))
