{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified System.IO  as IO
import qualified AdventOfCode.Grid as Grid

data Node = Infected | Clean | Flagged | Weakened deriving (Eq, Ord, Show)

getNode :: Grid.Pos -> Grid.Grid Node -> Node
getNode pos grid = fromMaybe Clean (M.lookup pos grid)

putNode :: Grid.Pos -> Node -> Grid.Grid Node -> Grid.Grid Node
putNode pos node grid = M.insert pos node grid

data Virus = Virus
    { vPos             :: !Grid.Pos
    , vDir             :: !Grid.Dir
    , vGrid            :: !(Grid.Grid Node)
    , vInfectedByBurst :: !Int
    }

zero :: Grid.Grid Node -> Virus
zero grid = Virus
    { vPos             = fromMaybe (error "no center") $ Grid.center grid
    , vDir             = Grid.U
    , vGrid            = grid
    , vInfectedByBurst = 0
    }

burst :: (Node -> Node) -> Virus -> Virus
burst f !v = v
    { vPos             = Grid.move dir (vPos v)
    , vDir             = dir
    , vGrid            = putNode (vPos v) next (vGrid v)
    , vInfectedByBurst =
        if next == Infected
            then vInfectedByBurst v + 1
            else vInfectedByBurst v
    }
  where
    !node = getNode (vPos v) (vGrid v)
    !next = f node
    !dir  = case node of
        Clean    -> Grid.turnLeft (vDir v)
        Weakened -> vDir v
        Infected -> Grid.turnRight (vDir v)
        Flagged  -> Grid.turnAround (vDir v)

bursts :: (Node -> Node) -> Int -> Virus -> Virus
bursts f n v = if n <= 0 then v else bursts f (n - 1) (burst f v)

basic :: Node -> Node
basic n = case n of
    Clean    -> Infected
    _        -> Clean

evolved :: Node -> Node
evolved n = case n of
    Clean    -> Weakened
    Weakened -> Infected
    Infected -> Flagged
    Flagged  -> Clean

main :: IO ()
main = do
    cgrid <- Grid.readGrid return IO.stdin
    let ngrid  = (\c -> case c of '#' -> Infected; _ -> Clean) <$> cgrid

    let virus0 = bursts basic 10000 (zero ngrid)
    putStrLn $ "Basic infected: " ++ show (vInfectedByBurst virus0)

    let virus1 = bursts evolved 10000000 (zero ngrid)
    putStrLn $ "Evolved infected: " ++ show (vInfectedByBurst virus1)
