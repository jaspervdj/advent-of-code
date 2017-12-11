{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import qualified System.IO  as IO

data Dir = U | R | D | L deriving (Bounded, Enum, Eq, Ord, Show)

turnRight, turnLeft :: Dir -> Dir
turnRight d = if d == maxBound then minBound else succ d
turnLeft  d = if d == minBound then maxBound else pred d

turnAround :: Dir -> Dir
turnAround U = D
turnAround R = L
turnAround D = U
turnAround L = R

data Pos = Pos !Int !Int deriving (Eq, Ord, Show)

move :: Dir -> Pos -> Pos
move dir (Pos x y) = case dir of
    U -> Pos x       (y - 1)
    R -> Pos (x + 1) y
    D -> Pos x       (y + 1)
    L -> Pos (x - 1) y

newtype Grid a = Grid (M.Map Pos a) deriving (Functor, Show)

center :: Grid a -> Pos
center (Grid grid) = case M.maxViewWithKey grid of
    Nothing                -> error "center: Empty grid"
    Just ((Pos x y, _), _) -> Pos (x `div` 2) (y `div` 2)

readGrid :: IO.Handle -> IO (Grid Char)
readGrid h = do
    ls <- lines <$> IO.hGetContents h
    return $ Grid $ M.fromList
        [ (Pos x y, c)
        | (y, l) <- zip [0 ..] ls
        , (x, c) <- zip [0 ..] l
        ]

data Node = Infected | Clean | Flagged | Weakened deriving (Eq, Ord, Show)

getNode :: Pos -> Grid Node -> Node
getNode pos (Grid grid) = fromMaybe Clean (M.lookup pos grid)

putNode :: Pos -> Node -> Grid Node -> Grid Node
putNode pos node (Grid grid) = Grid (M.insert pos node grid)

data Virus = Virus
    { vPos             :: !Pos
    , vDir             :: !Dir
    , vGrid            :: !(Grid Node)
    , vInfectedByBurst :: !Int
    }

zero :: Grid Node -> Virus
zero grid = Virus
    { vPos             = center grid
    , vDir             = U
    , vGrid            = grid
    , vInfectedByBurst = 0
    }

burst :: (Node -> Node) -> Virus -> Virus
burst f !v = v
    { vPos             = move dir (vPos v)
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
        Clean    -> turnLeft (vDir v)
        Weakened -> vDir v
        Infected -> turnRight (vDir v)
        Flagged  -> turnAround (vDir v)

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
    cgrid <- readGrid IO.stdin
    let ngrid  = (\c -> case c of '#' -> Infected; _ -> Clean) <$> cgrid

    let virus0 = bursts basic 10000 (zero ngrid)
    putStrLn $ "Basic infected: " ++ show (vInfectedByBurst virus0)

    let virus1 = bursts evolved 10000000 (zero ngrid)
    putStrLn $ "Evolved infected: " ++ show (vInfectedByBurst virus1)
