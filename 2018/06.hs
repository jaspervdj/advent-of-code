import qualified AdventOfCode.Grid as G
import           Control.Monad     (forM, guard)
import           Data.Char         (isDigit)
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe        (mapMaybe)
import           Data.Ord          (comparing)
import qualified System.IO         as IO
import           Text.Read         (readMaybe)

data Coordinate a = Coordinate
    { coordValue :: a
    , coordPos   :: G.Pos
    } deriving (Eq, Ord, Show)

parseCoordinates :: IO.Handle -> IO [Coordinate Int]
parseCoordinates h = do
    ls <- lines <$> IO.hGetContents h
    forM (zip [0 ..] ls) $ \(ident, line) ->
        case mapMaybe readMaybe (words (map space line)) of
            [x, y] -> return $ Coordinate ident (G.Pos x y)
            _      -> fail $ "Could not parse line: " ++ line
  where
    space c = if isDigit c then c else ' '

bounds :: Int -> [Coordinate a] -> (G.Pos, G.Pos)
bounds margin coordinates = (G.Pos minX minY, G.Pos maxX maxY)
  where
    minX = minimum [x | Coordinate _ (G.Pos x _) <- coordinates] - margin
    maxX = maximum [x | Coordinate _ (G.Pos x _) <- coordinates] + margin
    minY = minimum [y | Coordinate _ (G.Pos _ y) <- coordinates] - margin
    maxY = maximum [y | Coordinate _ (G.Pos _ y) <- coordinates] + margin

-- | A better `minimumBy`.
minimaBy :: (Ord a, Foldable t) => (a -> a -> Ordering) -> t a -> [a]
minimaBy f = L.foldl' step []
  where
    step []       x = [x]
    step (y : ys) x = case f x y of
        EQ -> x : y : ys
        LT -> [x]
        GT -> y : ys

-- | Assign positions witin the bounds to the closest coordinates.
assign :: Ord a => (G.Pos, G.Pos) -> [Coordinate a] -> [(G.Pos, a)]
assign (G.Pos minX minY, G.Pos maxX maxY) coordinates = do
    pos <- G.Pos <$> [minX .. maxX] <*> [minY .. maxY]
    let closest = minimaBy (comparing (G.manhattan pos . coordPos)) coordinates
    case closest of
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
safe (G.Pos minX minY, G.Pos maxX maxY) coordinates maxTotal = do
    pos <- G.Pos <$> [minX .. maxX] <*> [minY .. maxY]
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
