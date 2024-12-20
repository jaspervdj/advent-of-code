{-# LANGUAGE RecordWildCards #-}
import qualified AdventOfCode.Dijkstra as Dijkstra
import qualified AdventOfCode.Grid     as G
import           AdventOfCode.V2
import           Control.Monad         (guard)
import           Data.Foldable.Extra   (minimaBy)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (isJust, isNothing, listToMaybe,
                                        maybeToList)
import           Data.Ord              (comparing)
import qualified Data.Set              as S
import           Prelude               hiding (round)
import qualified System.IO             as IO

data Terrain = Cavern | Wall deriving (Eq, Ord, Show)

data Team = Goblin | Elf deriving (Eq, Ord, Show)

data Combatant = Combatant
    { cId     :: !G.Pos  -- We use starting position as immutable ID.
    , cTeam   :: !Team
    , cHealth :: !Int
    , cPower  :: !Int
    } deriving (Show)

data Battle = Battle
    { bCombatants :: !(G.Grid Combatant)
    , bTerrain    :: !(G.Grid Terrain)
    , bRound      :: !Int
    } deriving (Show)

readBattle :: IO.Handle -> IO Battle
readBattle h = do
    charGrid <- G.readGrid return h
    let bCombatants = M.mapMaybeWithKey parseCombatant charGrid
        bTerrain = M.mapMaybe parseTerrain charGrid
        bRound = 0
    return Battle {..}
  where
    parseCombatant p 'E' = Just (Combatant p Elf 200 3)
    parseCombatant p 'G' = Just (Combatant p Goblin 200 3)
    parseCombatant _ _   = Nothing

    parseTerrain '#' = Just Wall
    parseTerrain _   = Just Cavern

printBattle :: IO.Handle -> Battle -> IO ()
printBattle h Battle {..} = do
    IO.hPutStrLn h $ "After round " ++ show bRound ++ ":"

    G.printGrid h $ M.mapWithKey
        (\pos terrain -> case M.lookup pos bCombatants of
            Nothing -> case terrain of Cavern -> '.'; Wall -> '#'
            Just c  -> team (cTeam c))
        bTerrain

    IO.hPutStrLn h $ L.intercalate ", " $
        map (combatant . snd) $ readingOrder fst $ M.toList bCombatants
  where
    team Elf    = 'E'
    team Goblin = 'G'
    combatant c = team (cTeam c) : "(" ++ show (cHealth c) ++ ")"

readingOrder :: (a -> G.Pos) -> [a] -> [a]
readingOrder f = L.sortBy (comparing ((\(V2 x y) -> (y, x)) . f))

enemies :: Combatant -> Battle -> [(G.Pos, Combatant)]
enemies combatant Battle {..} =
    [(p, c) | (p, c) <- M.toList bCombatants, cTeam c /= cTeam combatant]

-- | Find an adjacent enemy to attack.
target :: (G.Pos, Combatant) -> Battle -> Maybe (G.Pos, Combatant)
target (pos, combatant) battle = listToMaybe $
    readingOrder fst $
    minimaBy (comparing (cHealth . snd)) $
    [ (p, e)
    | p <- G.neighbours pos
    , e <- maybeToList $ M.lookup p (bCombatants battle)
    , cTeam e /= cTeam combatant
    ]

-- | Play a round, and return if the stopping condition occurred during this
-- round.
round :: Battle -> (Bool, Battle)
round battle =
    takeTurns battle queue
  where
    queue = readingOrder fst $ M.toList $ bCombatants battle

    takeTurns battle0 [] = (False, battle0)
    takeTurns battle0 ((pos0, c0) : qs)
        -- Check if the queued combatant is still there.
        | Just c1 <- M.lookup pos0 (bCombatants battle0)
        , cId c1 == cId c0 =
            -- Check the completely stupid stopping condition.
            if null (enemies c1 battle0) then
                (True, battle0)
            else
                let ((pos1, c2), battle1) = move (pos0, c1) battle0
                    battle2 = attack (pos1, c2) battle1 in
                takeTurns battle2 qs
        -- If not they probably died.
        | otherwise = takeTurns battle0 qs

move :: (G.Pos, Combatant) -> Battle -> ((G.Pos, Combatant), Battle)
move (pos, combatant) battle
    -- Already adjacent to an enemy.
    | isJust $ target (pos, combatant) battle = ((pos, combatant), battle)

    -- There is a target we can go towards.
    | (t : _) <- goals =
        let distToGoal = Dijkstra.distances $ Dijkstra.dijkstra
                Dijkstra.defaultOptions
                    { Dijkstra.neighbours = neighbours
                    , Dijkstra.start      = S.singleton t
                    }
            next =
                map fst $ readingOrder fst $
                minimaBy (comparing snd) $
                [ (n, d)
                | n <- G.neighbours pos
                , d <- maybeToList $ M.lookup n distToGoal
                ] in

        case next of
            [] -> ((pos, combatant), battle)
            (npos : _) ->
                let combatants =
                        M.insert npos combatant $
                        M.delete pos (bCombatants battle) in
                ((npos, combatant), battle {bCombatants = combatants})

    -- Nowhere to move.
    | otherwise = ((pos, combatant), battle)
  where
    neighbours p = do
        q <- G.neighbours p
        terrain <- maybeToList $ M.lookup q (bTerrain battle)
        guard $ terrain == Cavern && not (q `M.member` bCombatants battle)
        pure (1, q)

    distToMe = Dijkstra.distances $ Dijkstra.dijkstra Dijkstra.defaultOptions
        { Dijkstra.neighbours = neighbours
        , Dijkstra.start      = S.singleton pos
        }

    -- The closest targets.
    goals =
        map fst $ readingOrder fst $
        minimaBy (comparing snd) $
        [ (p, dist)
        | (tp, _) <- enemies combatant battle
        , p       <- G.neighbours tp
        , M.lookup p (bTerrain battle) == Just Cavern
        , isNothing $ M.lookup p (bCombatants battle)
        , dist <- maybeToList $ M.lookup p distToMe
        ]

attack :: (G.Pos, Combatant) -> Battle -> Battle
attack (pos, combatant) battle = case target (pos, combatant) battle of
    -- No target to strike
    Nothing -> battle
    -- Can actually attack
    Just (tpos, tcomb0)
        | cHealth tcomb1 <= 0 ->
            battle {bCombatants = M.delete tpos (bCombatants battle)}
        | otherwise ->
            battle {bCombatants = M.insert tpos tcomb1 (bCombatants battle)}
      where
        tcomb1 = tcomb0 {cHealth = cHealth tcomb0 - cPower combatant}

outcome :: Battle -> Battle
outcome battle0 = case round battle0 of
    (True, battle1)  -> battle1
    (False, battle1) -> outcome (battle1 {bRound = bRound battle1 + 1})

checksum :: Battle -> Int
checksum b = bRound b * sum [cHealth c | (_, c) <- M.toList (bCombatants b)]

setElvesAttackPower :: Int -> Battle -> Battle
setElvesAttackPower n battle =
    battle {bCombatants = M.map setAttackPower (bCombatants battle)}
  where
    setAttackPower c = if cTeam c == Elf then c {cPower = n} else c

numElves :: Battle -> Int
numElves = length . filter (== Elf) . map (cTeam . snd) . M.toList . bCombatants

main :: IO ()
main = do
    -- Part 1
    battle <- readBattle IO.stdin
    let final = outcome battle
    printBattle IO.stderr final
    print $ checksum final

    -- Part 2
    let initialNumElves = numElves battle
        solution n =
            let b = outcome (setElvesAttackPower n battle) in
            if numElves b == initialNumElves then b else solution (n + 1)
        p2 = solution 4
    printBattle IO.stderr p2
    print $ checksum p2
