import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import           AdventOfCode.Parsing      (sections)
import           AdventOfCode.V2           (V2 (..))

parseGrids :: String -> Either String [G.Grid Bool]
parseGrids str =
    traverse G.fromString (map unlines $ sections str) >>=
    traverse (traverse parseChar)
  where
    parseChar c = case c of
        '#' -> pure True
        '.' -> pure False
        _   -> Left $ "Unknown char: " ++ show c


data Schematic = Lock [Int] | Key [Int] deriving (Show)

parseSchematic :: G.Grid Bool -> Either String Schematic
parseSchematic g
    | w < 2 || h < 2 = Left "too small"
    | count (V2 0 0) G.R == w = pure $ Lock
        [count (V2 x 1) G.D | x <- [0 .. w - 1]]
    | count (V2 0 (h - 1)) G.R == w = pure $ Key
        [count (V2 x (h - 2)) G.U | x <- [0 .. w - 1]]
    | otherwise = Left "neither lock nor key"
  where
    (w, h) = (G.gridWidth g, G.gridHeight g)
    count p d = case G.lookup p g of
        Just True -> 1 + count (G.move 1 d p) d
        _         -> 0

main :: IO ()
main = pureMain $ \str -> do
    grids <- parseGrids str
    schematics <- traverse parseSchematic grids
    let part1 = length
            [ (l, k)
            | Lock l <- schematics
            , Key k  <- schematics
            , all (<= 5) $ zipWith (+) l k
            ]
    pure (pure part1, pure "fin")
