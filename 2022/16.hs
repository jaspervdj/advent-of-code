{-# LANGUAGE TypeFamilies #-}
import           AdventOfCode.BranchAndBound
import qualified AdventOfCode.Dijkstra       as Dijkstra
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser     as NP
import           Control.Applicative         ((<|>))
import           Data.Foldable               (toList)
import qualified Data.Map                    as M

type ValveId = String

type Caves = M.Map ValveId (Int, [ValveId])

parseCaves :: NP.Parser Char Caves
parseCaves = M.fromList . toList <$> NP.sepBy1 valve NP.newline
  where
    valve = (\k f t -> (k, (f, t)))
        <$> (NP.string "Valve " *> valveId)
        <*> (NP.string " has flow rate=" *> NP.decimal <* NP.string ";")
        <*> (toList <$> NP.many1 (() <$ NP.lower <|> NP.char ' ') *> tunnels)
    tunnels = toList <$> NP.sepBy1 valveId (NP.string ", ")
    valveId = toList <$> NP.many1 NP.upper

type Distances = M.Map (ValveId, ValveId) Int

makeDistances :: Caves -> Distances
makeDistances input = M.fromList $ do
    (v0, _) <- M.toList input
    (d, path) <- M.toList . Dijkstra.bfsDistances $
        Dijkstra.bfs (snd . (input M.!)) (const False) v0
    pure ((v0, d), pred (length path))

type Minute = Int

data State = State
    { stateDistances :: Distances
    , stateClosed    :: M.Map ValveId Int
    , stateTimeLeft  :: Minute
    , stateScore     :: Int
    , statePosition  :: ValveId
    }

instance BranchAndBound State where
    type Score State = Int

    score = stateScore

    next s | stateTimeLeft s <= 0 = []
    next s = do
        (pos, flow) <- M.toList $ stateClosed s
        let dist     = stateDistances s M.! (statePosition s, pos)
            timeLeft = stateTimeLeft s - dist - 1
        pure s
            { stateClosed   = M.delete pos (stateClosed s)
            , stateTimeLeft = timeLeft
            , stateScore    = stateScore s + flow * timeLeft
            , statePosition = pos
            }

    potential s = stateScore s + stateTimeLeft s * sum (stateClosed s)

data ElephantState = ElephantState State Minute ValveId

instance BranchAndBound ElephantState where
    type Score ElephantState = Int

    score (ElephantState s _ _) = stateScore s

    next (ElephantState s delay pos) = do
        n <- next s
        let dt = stateTimeLeft s - stateTimeLeft n
        if dt < delay then pure $
            ElephantState n (delay - dt) pos
        else pure $
            ElephantState
            n {statePosition = pos, stateTimeLeft = stateTimeLeft s - delay}
            (dt - delay)
            (statePosition n)

    potential (ElephantState s _ _) = potential s

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseCaves str
    let distances = makeDistances input
        caves     = M.filter ((> 0)) $ fst <$> input
        state1    = State distances caves 30 0 "AA"
        part1     = score $ branchAndBound state1
        state2    = ElephantState state1 {stateTimeLeft = 26} 0 "AA"
        part2     = score $ branchAndBound state2
    pure (pure part1, pure part2)
