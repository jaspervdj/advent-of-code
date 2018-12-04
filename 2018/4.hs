{-# LANGUAGE LambdaCase #-}

import           Data.Function (on)
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified System.IO     as IO
import           Text.Read     (readMaybe)

--------------------------------------------------------------------------------
-- Datatypes

data Date = Date Int Int Int deriving (Eq, Ord, Show)
data Time = Time Int Int deriving (Eq, Ord, Show)

type Guard = Int

data Event
    = StartsShift Guard
    | WakesUp
    | FallsAsleep
    deriving (Eq, Ord, Show)

data Entry = Entry Date Time Event deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Parsing

parseEntry :: String -> IO Entry
parseEntry line = case words (map space line) of
    (y : m : d : h : t : rest)
        | Just date <- Date <$> readMaybe y <*> readMaybe m <*> readMaybe d
        , Just time <- Time <$> readMaybe h <*> readMaybe t -> case rest of
            ["wakes", "up"]             -> return $ Entry date time WakesUp
            ["falls", "asleep"]         -> return $ Entry date time FallsAsleep
            ["Guard", i, "begins", "shift"]
                | Just n <- readMaybe i -> return $
                    Entry date time $ StartsShift n
            _  -> fail $ "Could not parse event: " ++ line
    _ -> fail $ "Could not parse time: " ++ line
  where
    space c = if c `elem` "[-:]#" then ' ' else c

parseEntries :: IO.Handle -> IO [Entry]
parseEntries h = IO.hGetContents h >>= mapM parseEntry . lines

--------------------------------------------------------------------------------
-- Grouping by night

type Nap = (Int, Int)

data Night = Night Guard [Nap] deriving (Show)

byNight :: [Entry] -> [Night]
byNight (Entry _ _ (StartsShift guard) : entries0) =
    let (naps, entries1) = takeNaps entries0 in
    Night guard naps : byNight entries1

byNight (_ : entries0) = byNight entries0

byNight [] = []

takeNaps :: [Entry] -> ([Nap], [Entry])
takeNaps = \case
    (Entry _ (Time _ start) FallsAsleep) :
            (Entry _ (Time _ end) WakesUp) :
            entries0 ->
        let nap = (start, end)
            (naps, entries1) = takeNaps entries0 in
        (nap : naps, entries1)

    entries0 -> ([], entries0)

sleepiestGuard :: [Night] -> Guard
sleepiestGuard nights = fst $ L.maximumBy (compare `on` snd) $
    M.toList $ M.fromListWith (+)
    [ (g, end - start)
    | Night g naps <- nights
    , (start, end) <- naps
    ]

napVectorsByGuard :: [Night] -> [(Guard, V.Vector Int)]
napVectorsByGuard nights = M.toList $ M.fromListWith (V.zipWith (+)) $
    [ (g, napVector naps)
    | Night g naps <- nights
    ]
  where
    napVector naps =
        V.replicate 60 0 V.//
        [(i, 1) | (start, end) <- naps, i <- [start .. end - 1]]

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    entries <- L.sort <$> parseEntries IO.stdin
    let byNight' = byNight entries
        byGuard' = napVectorsByGuard byNight'

    -- Part 1

    let sleepiestGuard' = sleepiestGuard byNight'
        sleepiestGuardMinute' = maybe 0 V.maxIndex $
            lookup sleepiestGuard' byGuard'

    print $ sleepiestGuard' * sleepiestGuardMinute'

    -- Part 2

    let sleepiestMinuteByGuard = do
            (g, vec) <- byGuard'
            let minute = V.maxIndex vec
            return (vec V.! minute, (g, minute))

    print $ uncurry (*) $ snd $
        L.maximumBy (compare `on` fst) sleepiestMinuteByGuard
