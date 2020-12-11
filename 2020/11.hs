{-# LANGUAGE LambdaCase #-}
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.V2           (V2 (..), (.+.))
import           Data.Foldable             (toList)

data Tile = Empty | Vacant | Occupied deriving (Eq, Ord, Show)

parseTile :: Char -> Either String Tile
parseTile = \case
    '.' -> pure Empty
    'L' -> pure Vacant
    '#' -> pure Occupied
    c   -> Left $ "Unknown tile: " <> show c

step1 :: G.Grid Tile -> G.Grid Tile
step1 grid = flip G.mapWithKey grid $ \pos tile -> case tile of
        Vacant | occupied pos == 0   -> Occupied
        Occupied | occupied pos >= 4 -> Vacant
        _                            -> tile
  where
    occupied p =
        length [() | nb <- G.neighbours p, Just Occupied == G.lookup nb grid] +
        length [() | nb <- G.diagonal p, Just Occupied == G.lookup nb grid]

step2 :: G.Grid Tile -> G.Grid Tile
step2 grid = flip G.mapWithKey grid $ \pos tile -> case tile of
    Vacant | occupied pos == 0   -> Occupied
    Occupied | occupied pos >= 5 -> Vacant
    _                            -> tile
  where
    occupied pos = length [() | dir <- directions, Just Occupied == see pos dir]
    directions = [V2 x y | x <- [-1 .. 1] , y <- [-1 .. 1] , x /= 0 || y /= 0]
    see start dir =
        let go pos = case G.lookup pos grid of
                Nothing    -> Nothing
                Just Empty -> go (pos .+. dir)
                Just x     -> Just x in
        go (start .+. dir)

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let y = f x in if x == y then x else fixed f y

main :: IO ()
main = pureMain $ \input -> do
    grid0 <- G.fromString input >>= traverse parseTile
    let solve f = length . filter (== Occupied) . toList . fixed f
    pure (pure $ solve step1 grid0, pure $ solve step2 grid0)
