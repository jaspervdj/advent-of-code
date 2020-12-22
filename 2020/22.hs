import           AdventOfCode.Main
import           Data.Foldable     (toList)
import           Data.Sequence     (Seq (..), (|>))
import qualified Data.Sequence     as Seq
import           Data.Set          (Set)
import qualified Data.Set          as Set

type Decks = (Seq Int, Seq Int)

parseDecks :: String -> Decks
parseDecks inputstr =
    let (p1, p2) = break null $ lines inputstr in
    (Seq.fromList . map read $ drop 1 p1, Seq.fromList . map read $ drop 2 p2)

type Result = (Bool, Decks)

playNonRec :: Decks -> Result
playNonRec (x :<| xs, y :<| ys)
    | x > y     = playNonRec (xs |> x |> y, ys)
    | otherwise = playNonRec (xs, ys |> y |> x)
playNonRec (p1, p2) = (Seq.null p2, (p1, p2))

playRec :: Set Decks -> Decks -> Result
playRec _ decks@(Seq.Empty, _) = (False, decks)
playRec _ decks@(_, Seq.Empty) = (True, decks)
playRec prev decks | decks `Set.member` prev = (True, decks)
playRec prev r@(x :<| xs, y :<| ys) = playRec (Set.insert r prev) $
    if win then (xs |> x |> y, ys) else (xs, ys |> y |> x)
  where
    win | x > Seq.length xs || y > Seq.length ys = x > y
        | otherwise = fst $ playRec Set.empty (Seq.take x xs, Seq.take y ys)

score :: Result -> Int
score (win, (p1, p2)) =
    let d = if win then p1 else p2 in
    sum . zipWith (*) [1 ..] . toList $ Seq.reverse d

main :: IO ()
main = simpleMain $ \input ->
    let decks = parseDecks input in
    (score $ playNonRec decks, score $ playRec Set.empty decks)
