{-# LANGUAGE BangPatterns #-}

import qualified AdventOfCode.Loop as Loop
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import qualified Data.Set          as S
import qualified System.IO         as IO

type Pots = S.Set Int
type Rules = M.Map [Bool] Bool

showPots :: Pots -> String
showPots pots =
    [ if p `S.member` pots then '#' else '.'
    | p <- [lo .. hi]
    ]
  where
    (lo, hi) = range pots

range :: Pots -> (Int, Int)
range pots = fromMaybe (0, 0) $ do
    (lo, _) <- S.minView pots
    (hi, _) <- S.maxView pots
    return (lo, hi)

parseInput :: IO.Handle -> IO (Pots, Rules)
parseInput h = do
    pots  <- map plant . filter matters <$> IO.hGetLine h
    ""    <- IO.hGetLine h
    rules <- IO.hGetContents h >>= mapM parseRule . lines
    return (toSet pots, M.fromList rules)
  where
    matters = (`elem` ".#")
    space   = map $ \c -> if matters c then c else ' '
    plant   = (== '#')

    toSet = S.fromList . map fst . filter snd . zip [0 ..]

    parseRule line = case words (space line) of
        [lhs, [rhs]] -> return (map plant lhs, plant rhs)
        _            -> fail $ "Could not parse line: " ++ line

generation :: Rules -> Pots -> Pots
generation rules pots = S.fromList
    [ p
    | p <- [lo - 2 .. hi + 2]
    , fromMaybe False $ M.lookup
        [get (p - 2), get (p - 1), get p, get (p + 1), get (p + 2)]
        rules
    ]
  where
    get      = (`S.member` pots)
    (lo, hi) = range pots

main :: IO ()
main = do
    (initial, rules) <- parseInput IO.stdin
    let generations = iterate (generation rules) initial
    print $ sum $ generations !! 20

    let loop = Loop.findLoop showPots (generation rules) initial

        -- It turns out that `lLength = 1`, which makes things a lot simpler.
        -- We assume this is the case for all inputs.
        shift =
            let (lo1, _) = range (Loop.lFirst loop)
                (lo2, _) = range (Loop.lSecond loop) in
            lo2 - lo1

        totalShift = shift * (50000000000 - Loop.lStart loop)
    print $ sum $ map (+ totalShift) $
        S.toList $ generations !! Loop.lStart loop
