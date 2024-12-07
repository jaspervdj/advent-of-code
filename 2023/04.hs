{-# LANGUAGE BangPatterns #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.Foldable           (toList)
import           Data.Monoid             (Sum (..))
import qualified Data.Set                as S

data Card = Card Int (S.Set Int) [Int] deriving (Show)

parseCards :: NP.Parser Char [Card]
parseCards = fmap toList $ NP.many1 $ Card
    <$> (NP.string "Card" *> NP.spaces *> NP.decimal <* NP.char ':')
    <*> (S.fromList <$> (NP.spaces *> numbers))
    <*> (NP.char '|' *> NP.spaces *> numbers)
  where
    numbers = toList <$> NP.many1 (NP.decimal <* NP.spaces)

cardMatches :: Card -> Int
cardMatches (Card _ winning have) = length $ filter (`S.member` winning) have

cardPoints :: Card -> Int
cardPoints card = let m = cardMatches card in if m <= 0 then 0 else 2 ^ (m - 1)

-- Infinite Monoid List, some elements followed by repeated mempty
-- This lets us easily zip the copies together with `mappend = +`
newtype Iml m = Iml {runIml :: [m]}

instance Semigroup m => Semigroup (Iml m) where
    Iml []       <> Iml ys       = Iml ys
    Iml xs       <> Iml []       = Iml xs
    Iml (x : xs) <> Iml (y : ys) = Iml $ x <> y : runIml (Iml xs <> Iml ys)

instance Monoid m => Monoid (Iml m) where mempty = Iml []

popIml :: Monoid m => Iml m -> (m, Iml m)
popIml (Iml [])       = (mempty, Iml [])
popIml (Iml (x : xs)) = (x,      Iml xs)

cardCopies :: [Card] -> Int
cardCopies = getSum . go mempty 0
  where
    go _      !acc []             = acc
    go copies !acc (card : cards) =
        let (n, old) = popIml copies
            new      = Iml $ replicate (cardMatches card) (n <> 1) in
        go (old <> new) (acc <> 1 <> n) cards

main :: IO ()
main = pureMain $ \input -> do
    cards <- NP.runParser parseCards input
    pure (pure (sum $ map cardPoints cards), pure (cardCopies cards))
