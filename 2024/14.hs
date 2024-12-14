import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main       (ioMain)
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..), (.*), (.+.))
import           Control.Applicative     (many)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, listToMaybe, maybeToList)
import qualified System.IO               as IO

data Robot = Robot
    { rPos :: V2 Int
    , rVel :: V2 Int
    } deriving (Show)

parseRobot :: NP.Parser Char Robot
parseRobot = Robot
    <$> (NP.string "p=" *> v2 <* NP.spaces)
    <*> (NP.string "v=" *> v2 <* NP.spaces)
  where
    v2 = V2 <$> NP.signedDecimal <* NP.char ',' <*> NP.signedDecimal

data Area = Area Int Int deriving (Show)

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Show)

quadrant :: Area -> V2 Int -> Maybe Quadrant
quadrant (Area w h) (V2 x y) =
    case (compare x (w `div` 2), compare y (h `div` 2)) of
        (GT, LT) -> Just Q1
        (LT, LT) -> Just Q2
        (LT, GT) -> Just Q3
        (GT, GT) -> Just Q4
        _        -> Nothing

wrap :: Area -> V2 Int -> V2 Int
wrap (Area w h) (V2 x y) = V2 (x `mod` w) (y `mod` h)

move :: Area -> Int -> [Robot] -> [Robot]
move area seconds robots =
    [ Robot (wrap area $ p .+. v .* seconds) v
    | Robot p v <- robots
    ]

byQuadrant :: Area -> [Robot] -> M.Map Quadrant [Robot]
byQuadrant area robots = M.fromListWith (++) $ do
    robot <- robots
    q <- maybeToList $ quadrant area (rPos robot)
    pure (q, [robot])

toGrid :: [Robot] -> G.Grid Int
toGrid robots = M.fromListWith (+) [(rPos r, 1) | r <- robots]

-- Sum of the number of neighbours a robot has, over all robots.
coziness :: G.Grid Int -> Int
coziness grid = sum
    [ n * m
    | (p, n) <- M.toList grid
    , q <- G.neighbours p
    , m <- maybeToList $ M.lookup q grid
    ]

main :: IO ()
main = ioMain $ \input -> do
    robots <- either fail pure $ NP.runParser (many parseRobot) input
    let area = Area 101 103
        part1 = product $ fmap length $ byQuadrant area $ move area 100 robots
        threshold = length robots  -- Sort of arbitrary
        part2 = fromMaybe 0 $ listToMaybe $ dropWhile
            (\s -> coziness (toGrid $ move area s robots) < threshold)
            [1 ..]
        picture = G.toString $ fmap (\r -> if r > 0 then '#' else '.') $
            toGrid $ move area part2 robots
    pure (pure part1, IO.hPutStr IO.stderr picture >> pure part2)
