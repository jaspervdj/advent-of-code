import qualified AdventOfCode.Grid    as G
import qualified AdventOfCode.Parsing as Parsing
import           AdventOfCode.V2
import           Control.Monad        (forM, guard)
import           Data.Foldable.Extra  (minimaBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Ord             (comparing)
import qualified System.IO            as IO

data Coordinate a = Coordinate
    { coordValue :: a
    , coordPos   :: G.Pos
    } deriving (Eq, Ord, Show)

parseCoordinates :: IO.Handle -> IO [Coordinate Int]
parseCoordinates h = do
    ls <- lines <$> IO.hGetContents h
    forM (zip [0 ..] ls) $ \(ident, line) ->
        case Parsing.ints line of
            [x, y] -> return $ Coordinate ident (V2 x y)
            _      -> fail $ "Could not parse line: " ++ line

bounds :: Int -> [Coordinate a] -> (G.Pos, G.Pos)
bounds margin coordinates = (V2 minX minY, V2 maxX maxY)
  where
    minX = minimum [x | Coordinate _ (V2 x _) <- coordinates] - margin
    maxX = maximum [x | Coordinate _ (V2 x _) <- coordinates] + margin
    minY = minimum [y | Coordinate _ (V2 _ y) <- coordinates] - margin
    maxY = maximum [y | Coordinate _ (V2 _ y) <- coordinates] + margin

-- | Assign positions witin the bounds to the closest coordinates.
assign :: Ord a => (G.Pos, G.Pos) -> [Coordinate a] -> [(G.Pos, a)]
assign (V2 minX minY, V2 maxX maxY) coordinates = do
    pos <- V2 <$> [minX .. maxX] <*> [minY .. maxY]
    let close = minimaBy (comparing (G.manhattan pos . coordPos)) coordinates
    case close of
        [Coordinate val _] -> return (pos, val)
        _                  -> []

-- | Count occurrences.
frequencies :: Ord a => [a] -> M.Map a Int
frequencies = M.fromListWith (+) . map (\x -> (x, 1))

-- | Keep only keys that are consistent in both input maps.
consistent :: (Eq v, Ord k) => M.Map k v -> M.Map k v -> M.Map k v
consistent l = M.mapMaybeWithKey $ \k x -> case M.lookup k l of
    Just y | x == y -> Just x
    _               -> Nothing

-- | Find safe positions.
safe :: (G.Pos, G.Pos) -> [Coordinate a] -> Int -> [G.Pos]
safe (V2 minX minY, V2 maxX maxY) coordinates maxTotal = do
    pos <- V2 <$> [minX .. maxX] <*> [minY .. maxY]
    let total = sum [G.manhattan pos coord | Coordinate _ coord <- coordinates]
    guard $ total < maxTotal
    return pos

main :: IO ()
main = do
    coords <- parseCoordinates IO.stdin
    let count1 = frequencies $ map snd $ assign (bounds 0 coords) coords
        count2 = frequencies $ map snd $ assign (bounds 1 coords) coords
        total  = consistent count1 count2

    print $ L.maximum $ map snd $ M.toList total
    print $ length $ safe (bounds 0 coords) coords 10000
