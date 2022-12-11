{-# LANGUAGE RecordWildCards #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import qualified Data.Foldable           as F
import           Data.List               (foldl', sortOn)
import qualified Data.Map                as M
import           Data.Ord                (Down (..))
import qualified Data.Sequence           as Seq
import           Prelude                 hiding (round)

data MonkeyExpr
    = Old
    | Lit !Int
    | Multiply !MonkeyExpr !MonkeyExpr
    | Add !MonkeyExpr !MonkeyExpr
    deriving (Show)

evalMonkeyExpr :: Int -> MonkeyExpr -> Int
evalMonkeyExpr old Old            = old
evalMonkeyExpr _   (Lit n)        = n
evalMonkeyExpr old (Multiply x y) = evalMonkeyExpr old x * evalMonkeyExpr old y
evalMonkeyExpr old (Add x y)      = evalMonkeyExpr old x + evalMonkeyExpr old y

type MonkeyId = Int

data Monkey = Monkey
    { monkeyId               :: !MonkeyId
    , monkeyItems            :: !(Seq.Seq Int)
    , monkeyOperation        :: !MonkeyExpr
    , monkeyTestDivBy        :: !Int
    , monkeyTestIfTrueThrow  :: !Int
    , monkeyTestIfFalseThrow :: !Int
    , monkeyInpections       :: !Int
    } deriving (Show)

parseMonkey :: NP.Parser Char Monkey
parseMonkey = Monkey
    <$> (NP.string "Monkey " *> NP.decimal <* NP.char ':' <* NP.spaces)
    <*> (NP.string "Starting items: " *> items <* NP.spaces)
    <*> (NP.string "Operation: new = " *> expr <* NP.spaces)
    <*> (NP.string "Test: divisible by " *> NP.decimal <* NP.spaces)
    <*> (NP.string "If true: throw to monkey " *> NP.decimal <* NP.spaces)
    <*> (NP.string "If false: throw to monkey " *> NP.decimal <* NP.spaces)
    <*> pure 0
  where
    expr = (NP.string "old" *> NP.spaces *> operator <* NP.spaces) <*> term
    term = (Old <$ NP.string "old") <|> (Lit <$> NP.decimal)
    operator = (Multiply Old <$ NP.char '*') <|> (Add Old <$ NP.char '+')

    items = Seq.fromList <$> NP.sepBy1 NP.decimal (NP.string ", ")

type Monkeys = M.Map MonkeyId Monkey

parseMonkeys :: NP.Parser Char Monkeys
parseMonkeys =
    M.fromList . fmap (\m -> (monkeyId m, m)) <$> NP.many1 parseMonkey

inspect :: (Int -> Int) -> Int -> Monkey -> Monkeys -> Monkeys
inspect manageWorryLevel item0 me monkeys = M.adjust
    (\m -> m {monkeyItems = monkeyItems m Seq.|> item1})
    destination
    monkeys
  where
    item1 = manageWorryLevel $ evalMonkeyExpr item0 (monkeyOperation me)
    destination
        | item1 `mod` monkeyTestDivBy me == 0 = monkeyTestIfTrueThrow me
        | otherwise                           = monkeyTestIfFalseThrow me

turn :: (Int -> Int) -> MonkeyId -> Monkeys -> Monkeys
turn manageWorryLevel mid monkeys0 = foldl'
    (\mnks item -> inspect manageWorryLevel item monkey1 mnks)
    (M.insert mid monkey1 monkeys0)
    (monkeyItems monkey0)
  where
    monkey0 = monkeys0 M.! mid
    monkey1 = monkey0
        { monkeyItems      = Seq.empty
        , monkeyInpections =
            monkeyInpections monkey0 + length (monkeyItems monkey0)
        }

round :: (Int -> Int) -> Monkeys -> Monkeys
round manageWorryLevel monkeys0 =
    foldl' (flip (turn manageWorryLevel)) monkeys0 . map fst $
    M.toAscList monkeys0

main :: IO ()
main = pureMain $ \input -> do
    monkeys0 <- NP.runParser parseMonkeys input
    let part1 = product . take 2 .
            sortOn Down . F.toList . fmap monkeyInpections $
            iterate (round (`div` 3)) monkeys0 !! 20
        ring  = product $ fmap monkeyTestDivBy monkeys0
        part2 = product . take 2 .
            sortOn Down . F.toList . fmap monkeyInpections $
            iterate (round (`mod` ring)) monkeys0 !! 10000
    pure (pure part1, pure part2)
