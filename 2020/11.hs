{-# LANGUAGE LambdaCase #-}
import qualified AdventOfCode.Grid   as G
import           AdventOfCode.V2     (V2 (..), zero, (.+.))
import qualified AdventOfCode.V2.Box as Box
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified System.IO           as IO

data Seat = Vacant | Occupied deriving (Eq, Ord, Show)

parseSeat :: Char -> IO (Maybe Seat)
parseSeat = \case
    '.' -> pure Nothing
    'L' -> pure $ Just Vacant
    '#' -> pure $ Just Occupied
    c   -> fail $ "Unknown tile: " <> show c

step1 :: G.Grid Seat -> G.Grid Seat
step1 grid = flip Map.mapWithKey grid $ \pos tile -> case tile of
    Vacant | occupied pos == 0   -> Occupied
    Occupied | occupied pos >= 4 -> Vacant
    _                            -> tile
  where
    neighbours = G.neighbours <> G.diagonal
    occupied pos = length
        [() | nb <- neighbours pos, Just Occupied == Map.lookup nb grid]

step2 :: G.Grid Seat -> G.Grid Seat
step2 grid = flip Map.mapWithKey grid $ \pos tile -> case tile of
    Vacant | occupied pos == 0   -> Occupied
    Occupied | occupied pos >= 5 -> Vacant
    _                            -> tile
  where
    bounds = fromMaybe (Box.fromV2 zero) $ G.box grid
    occupied pos = length [() | dir <- directions, Just Occupied == see pos dir]
    directions = [V2 x y | x <- [-1 .. 1] , y <- [-1 .. 1] , x /= 0 || y /= 0]
    see start dir =
        let go pos
                | not (pos `Box.inside` bounds) = Nothing
                | Just n <- Map.lookup pos grid = Just n
                | otherwise                     = go (pos .+. dir) in
        go (start .+. dir)

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let y = f x in if x == y then x else fixed f y

main :: IO ()
main = do
    grid0 <- Map.mapMaybe id <$> G.readGrid parseSeat IO.stdin
    let solve f = length . filter (== Occupied) . map snd . Map.toList . fixed f
    print $ solve step1 grid0
    print $ solve step2 grid0
