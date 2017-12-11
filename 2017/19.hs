{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad (msum)
import           Data.Char     (isAlpha)
import qualified Data.Map      as M
import           Data.Maybe    (maybeToList)
import qualified System.IO     as IO

data Dir = U | L | D | R deriving (Eq, Ord, Show)

turn :: Dir -> [Dir]
turn dir = case dir of
    U -> [L, R]
    L -> [U, D]
    D -> [L, R]
    R -> [U, D]

data Pos = Pos !Int !Int deriving (Eq, Ord, Show)

move :: Dir -> Pos -> Pos
move dir (Pos x y) = case dir of
    U -> Pos x       (y - 1)
    L -> Pos (x - 1) y
    D -> Pos x       (y + 1)
    R -> Pos (x + 1) y

type Grid a = M.Map Pos a

data Cell = Horizontal | Vertical | Crossroads | Character !Char
    deriving (Eq, Ord, Show)

parseCell :: Char -> Maybe Cell
parseCell '-' = Just Horizontal
parseCell '|' = Just Vertical
parseCell '+' = Just Crossroads
parseCell x   = if isAlpha x then Just (Character x) else Nothing

readGrid :: (Char -> Maybe a) -> IO.Handle -> IO (Grid a)
readGrid f h = do
    ls <- lines <$> IO.hGetContents h
    return $ M.fromList
        [ (Pos x y, v)
        | (y, l) <- zip [0 ..] ls
        , (x, c) <- zip [0 ..] l
        , v      <- maybeToList (f c)
        ]

data Dude = Dude
    { dPos       :: !Pos
    , dDir       :: !Dir
    , dCollected :: ![Char]
    , dSteps     :: !Int
    }

zeroDude :: Grid a -> Dude
zeroDude g =
    case [Pos x 0 | x <- [0 ..], _ <- maybeToList $ M.lookup (Pos x 0) g] of
        []      -> error "zeroDude: no position found"
        (p : _) -> Dude p D [] 1

stepDude :: Grid Cell -> Dude -> Maybe Dude
stepDude grid = \dude -> msum $ do
    dir <- dDir dude : turn (dDir dude)
    let !pos   = move dir (dPos dude)
        !steps = dSteps dude + 1

    cell <- maybeToList $ M.lookup pos grid
    let !collected = case cell of
            Character c -> c : dCollected dude
            Horizontal  -> dCollected dude
            Vertical    -> dCollected dude
            Crossroads  -> dCollected dude

    return $ Just dude
        { dPos       = pos
        , dSteps     = steps
        , dDir       = dir
        , dCollected = collected
        }

walkDude :: Grid Cell -> Dude -> Dude
walkDude grid dude = maybe dude (walkDude grid) (stepDude grid dude)

main :: IO ()
main = do
    grid <- readGrid parseCell IO.stdin
    let dude = walkDude grid (zeroDude grid)
    putStrLn $ "Collected: " ++ reverse (dCollected dude)
    putStrLn $ "Steps: " ++ show (dSteps dude)
