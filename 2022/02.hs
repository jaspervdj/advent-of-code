import           AdventOfCode.Main
import           Control.Monad     (guard)

data Result = Win | Draw | Lose deriving (Eq)

data Shape = Rock | Paper | Scissors deriving (Eq)

play :: Shape -> Shape -> Result
play Rock     Scissors = Win
play Paper    Rock     = Win
play Scissors Paper    = Win
play x        y        = if x == y then Draw else Lose

score :: Shape -> Shape -> Int
score you other = shapeScore you + resultScore (play you other)
  where
    shapeScore Rock     = 1
    shapeScore Paper    = 2
    shapeScore Scissors = 3
    resultScore Win  = 6
    resultScore Draw = 3
    resultScore Lose = 0

main :: IO ()
main = simpleMain $ \input ->
    let matches = do
            line <- lines input
            let [abc, xyz] = words line
            pure (abc, xyz)
        part1 = sum $ do
            (abc, xyz) <- matches
            pure $ score (parseXyz1 xyz) (parseAbc abc)
        part2 = sum $ do
            (abc, xyz) <- matches
            let other     = parseAbc abc
                objective = parseXyz2 xyz
                choice    = head $ do
                    x <- [Rock, Paper, Scissors]
                    guard $ play x other == objective
                    pure x
            pure $ score choice other in
    (part1, show part2)
  where
    parseAbc "A" = Rock
    parseAbc "B" = Paper
    parseAbc "C" = Scissors
    parseAbc _   = error "bad abc"

    parseXyz1 "X" = Rock
    parseXyz1 "Y" = Paper
    parseXyz1 "Z" = Scissors
    parseXyz1 _   = error "bad xyz"

    parseXyz2 "X" = Lose
    parseXyz2 "Y" = Draw
    parseXyz2 "Z" = Win
    parseXyz2 _   = error "bad xyz"
