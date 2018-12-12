{-# LANGUAGE BangPatterns #-}

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as S
import qualified System.IO  as IO

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

data Loop = Loop
    { pStart  :: !Int
    , pLength :: !Int
    , pShift  :: !Int
    } deriving (Show)

findLoop :: [Pots] -> Maybe Loop
findLoop = go M.empty 0
  where
    go _ _ [] = Nothing
    go memory !i (pots : potss) = case M.lookup (showPots pots) memory of
        Just (iteration, lo') -> Just $ Loop iteration (i - iteration) (lo - lo')
        Nothing -> go
            (M.insert (showPots pots) (i, lo) memory) (i + 1) potss
      where
        (lo, _) = range pots

main :: IO ()
main = do
    (initial, rules) <- parseInput IO.stdin
    let generations = iterate (generation rules) initial
    print $ sum $ generations !! 20

    let Just loop = findLoop generations
    -- It turns out that `pLength = 1`, which makes things a lot simpler.  We
    -- assume this is the case for all inputs.
    --
    let shift = pShift loop * (50000000000 - pStart loop)
    print $ sum $ map (+ shift) $ S.toList $ generations !! pStart loop
