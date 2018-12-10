{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

import qualified AdventOfCode.Grid as Grid
import           AdventOfCode.V2
import           Control.Monad     (msum)
import           Data.Char         (isAlpha)
import qualified Data.Map          as M
import           Data.Maybe        (maybeToList)
import qualified System.IO         as IO

data Cell = Horizontal | Vertical | Crossroads | Character !Char
    deriving (Eq, Ord, Show)

parseCell :: Char -> IO (Maybe Cell)
parseCell '-' = return (Just Horizontal)
parseCell '|' = return (Just Vertical)
parseCell '+' = return (Just Crossroads)
parseCell ' ' = return Nothing
parseCell x   | isAlpha x = return (Just (Character x))
parseCell x   = fail $ "Unknown cell: " ++ show x

data Dude = Dude
    { dPos       :: !Grid.Pos
    , dDir       :: !Grid.Dir
    , dCollected :: ![Char]
    , dSteps     :: !Int
    }

zeroDude :: Grid.Grid a -> Dude
zeroDude g = case candidates of
    []      -> error "zeroDude: no position found"
    (p : _) -> Dude p Grid.D [] 1
  where
    candidates =
        [ V2 x 0
        | x <- [0 ..], _ <- maybeToList $ M.lookup (V2 x 0) g
        ]

stepDude :: Grid.Grid Cell -> Dude -> Maybe Dude
stepDude grid = \dude -> msum $ do
    dir <- [dDir dude, Grid.turnLeft (dDir dude), Grid.turnRight (dDir dude)]
    let !pos   = Grid.move dir (dPos dude)
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

walkDude :: Grid.Grid Cell -> Dude -> Dude
walkDude grid dude = maybe dude (walkDude grid) (stepDude grid dude)

main :: IO ()
main = do
    grid <- M.mapMaybe id <$> Grid.readGrid parseCell IO.stdin
    let dude = walkDude grid (zeroDude grid)
    putStrLn $ "Collected: " ++ reverse (dCollected dude)
    putStrLn $ "Steps: " ++ show (dSteps dude)
