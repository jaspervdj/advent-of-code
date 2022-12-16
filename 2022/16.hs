import           AdventOfCode.Main
import qualified AdventOfCode.Dijkstra as Dijkstra
import Data.List (nub, foldl')
import Control.Monad (guard)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))

type ValveId = String

type Caves = M.Map ValveId (Int, [ValveId])

parseCaves :: NP.Parser Char Caves
parseCaves = M.fromList <$> NP.sepBy1 valve NP.newline
  where
    valve = (\k f t -> (k, (f, t)))
        <$> (NP.string "Valve " *> valveId)
        <*> (NP.string " has flow rate=" *> NP.decimal <* NP.string ";")
        <*> (NP.many1 (() <$ NP.lower <|> NP.char ' ') *> tunnels)
    tunnels = NP.sepBy1 valveId (NP.string ", ")
    valveId = NP.many1 NP.upper

type Distances = M.Map (ValveId, ValveId) Int

makeDistances :: Caves -> Distances
makeDistances input = M.fromList $ do
    (v0, _) <- M.toList input
    (d, path) <- M.toList . Dijkstra.bfsDistances $
        Dijkstra.bfs (snd . (input M.!)) (const False) v0
    pure ((v0, d), pred (length path))

type Minute = Int

data State = Valid (S.Set ValveId) Int | Invalid deriving (Show)

best :: [State] -> State
best []         = Invalid
best [s]        = s
best (Invalid : ss) = best ss
best (Valid o s : Invalid : ss) = best (Valid o s : ss)
best (Valid o0 s0 : Valid o1 s1 : ss)
    | s0 > s1   = best (Valid o0 s0 : ss)
    | otherwise = best (Valid o1 s1 : ss)

fill1 :: Caves -> Distances -> Minute -> ValveId -> M.Map (Minute, ValveId) State
fill1 input distances time start = mem
  where
    mem  = M.fromList [(k, comp k) | k <- keys]
    keys = (,) <$> [0 .. time] <*> (fst <$> M.toList input)

    comp (minute, loc)
        | minute <= 0 = if loc == start then Valid S.empty 0 else Invalid
        | otherwise   = best $ do
            prev <- fst <$> M.toList input
            let dist = distances M.! (prev, loc)
            case M.lookup (minute - 1 - dist, prev) mem of
                Nothing -> []
                Just Invalid -> []
                Just (Valid open score) -> do
                    guard . not $ loc `S.member` open
                    let timeLeft  = time - minute
                        (flow, _) = input M.! loc
                        pressure  = flow * timeLeft
                    pure $ Valid (S.insert loc open) (score + pressure)

data Search = Search
    { searchDistances :: Distances
    , searchClosed    :: M.Map ValveId Int
    , searchTimeLeft  :: Minute
    , searchScore     :: Int
    , searchPosition  :: ValveId
    }

bestScore :: Search -> Search -> Search
bestScore s0 s1 = if searchScore s0 > searchScore s1 then s0 else s1

next :: Search -> [Search]
next s | searchTimeLeft s <= 0 = []
next s = do
    (next, flow) <- M.toList $ searchClosed s
    let dist     = searchDistances s M.! (searchPosition s, next)
        timeLeft = searchTimeLeft s - dist - 1
    pure s
        { searchClosed   = M.delete next (searchClosed s)
        , searchTimeLeft = timeLeft
        , searchScore    = searchScore s + flow * timeLeft
        , searchPosition = next
        }

potential :: Search -> Int
potential s = (searchScore s +) $ sum $ do
    (next, flow) <- M.toList $ searchClosed s
    let dist     = searchDistances s M.! (searchPosition s, next)
        timeLeft = searchTimeLeft s - dist - 1
    pure $ flow * searchTimeLeft s

bruteforce :: Caves -> Distances -> Minute -> ValveId -> Int
bruteforce input distances time start =
    searchScore $
    let search0 = Search distances (fst <$> input) time 0 start in
    go search0 search0
  where
    go best0 current
        | potential current < searchScore best0 = best0
        | otherwise                             =
            foldl' go (bestScore best0 current) (next current)

data PairSearch = PairSearch Search Minute ValveId

pairScore :: PairSearch -> Int
pairScore (PairSearch s _ _) = searchScore s

pairBest :: PairSearch -> PairSearch -> PairSearch
pairBest x y = if pairScore x > pairScore y then x else y

pairNext :: PairSearch -> [PairSearch]
pairNext (PairSearch s delay wait) = do
    n <- next s
    let dt = searchTimeLeft s - searchTimeLeft n
    if dt < delay then
        pure $ PairSearch n (delay - dt) wait
    else
        let delay' = dt - delay in
        pure $ PairSearch
            n {searchPosition = wait, searchTimeLeft = searchTimeLeft s - delay}
            (dt - delay)
            (searchPosition n)

pairPotential :: PairSearch -> Int
pairPotential (PairSearch s _ _) = (searchScore s +) $ sum $ do
    (_, flow) <- M.toList $ searchClosed s
    pure $ flow * searchTimeLeft s

bruteforce2 :: Caves -> Distances -> Minute -> ValveId -> Int
bruteforce2 input distances time start =
    pairScore $
    let search0 = PairSearch (Search distances (fst <$> input) time 0 start) 0 start in
    go search0 search0
  where
    go best0 current
        | pairPotential current < pairScore best0 = best0
        | otherwise                             =
            foldl' go (pairBest best0 current) (pairNext current)

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseCaves str
    let distances = makeDistances input
        caves = M.filter ((> 0) . fst) input
        Valid _ part1 = best . map snd . M.toList $ fill1 input distances 30 "AA"
        part1' = bruteforce caves distances 30 "AA"
        part2 = bruteforce2 caves distances 26 "AA"
        -- Valid _ part2 = best . map snd . M.toList $ fill2 input distances 26 "AA"
    pure (pure (show (part1, part1')), pure part2)
