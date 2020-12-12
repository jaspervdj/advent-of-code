{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main
import           AdventOfCode.V2   (V2 (..), (.*), (.+.))
import           Data.List         (foldl')
import           Text.Read         (readMaybe)

data Instruction
    = Move !G.Dir !Int
    | TurnLeft
    | TurnAround
    | TurnRight
    | Forward !Int
    deriving (Show)

parseInstruction :: String -> Either String Instruction
parseInstruction = maybe (Left "Invalid instruction") Right . \case
    'N' : str -> Move G.U <$> readMaybe str
    'S' : str -> Move G.D <$> readMaybe str
    'E' : str -> Move G.R <$> readMaybe str
    'W' : str -> Move G.L <$> readMaybe str
    "L90"     -> pure TurnLeft
    "L180"    -> pure TurnAround
    "L270"    -> pure TurnRight
    "R90"     -> pure TurnRight
    "R180"    -> pure TurnAround
    "R270"    -> pure TurnLeft
    'F' : str -> Forward <$> readMaybe str
    _         -> Nothing

data Ship = Ship {shipDir :: !G.Dir, shipPos :: !G.Pos} deriving (Show)

stepShip :: Instruction -> Ship -> Ship
stepShip instruction ship@Ship {..} = case instruction of
    Move dir n -> ship {shipPos = G.move n dir shipPos}
    TurnLeft   -> ship {shipDir = G.turnLeft shipDir}
    TurnAround -> ship {shipDir = G.turnAround shipDir}
    TurnRight  -> ship {shipDir = G.turnRight shipDir}
    Forward n  -> ship {shipPos = G.move n shipDir shipPos}

data Waypoint = Waypoint {wpOffset :: !G.Pos, wpShip :: !G.Pos} deriving (Show)

stepWaypoint :: Instruction -> Waypoint -> Waypoint
stepWaypoint instruction wp@Waypoint {..} = case instruction of
    Move dir n -> wp {wpOffset = G.move n dir wpOffset}
    TurnLeft   -> case wpOffset of V2 x y -> wp {wpOffset = V2 y (-x)}
    TurnAround -> case wpOffset of V2 x y -> wp {wpOffset = V2 (-x) (-y)}
    TurnRight  -> case wpOffset of V2 x y -> wp {wpOffset = V2 (-y) x}
    Forward n  -> wp {wpShip = wpShip .+. wpOffset .* n}

main :: IO ()
main = pureMain $ \input -> do
    instrs <- traverse parseInstruction (lines input)
    pure (pure $ part1 instrs, pure $ part2 instrs)
  where
    ship0 = Ship G.R G.origin
    wp0   = Waypoint (V2 10 (-1)) G.origin
    part1 = G.manhattan G.origin . shipPos . foldl' (flip stepShip) ship0
    part2 = G.manhattan G.origin . wpShip . foldl' (flip stepWaypoint) wp0
