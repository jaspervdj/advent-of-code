import qualified AdventOfCode.Bfs          as Bfs
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main         (pureMain)
import qualified Data.Map                  as M
import           Data.Maybe                (maybeToList)
import qualified Data.Set                  as S

data Tile = Empty | Wall | Start | End deriving (Show, Eq)

parseTile :: Char -> Either String Tile
parseTile c = case c of
    '.' -> pure Empty
    '#' -> pure Wall
    'S' -> pure Start
    'E' -> pure End
    _   -> Left $ "Unknown char: " ++ show c

data Track = Track
    { tStart :: G.Pos
    , tEnd   :: G.Pos
    , tGrid  :: G.Grid Tile
    } deriving (Show)

parseTrack :: String -> Either String Track
parseTrack str = do
    grid <- G.fromString str >>= traverse parseTile
    start <- case map fst $ filter ((== Start) . snd) $ G.toList grid of
        [p] -> pure p
        _   -> Left "Unclear start"
    end <- case map fst $ filter ((== End) . snd) $ G.toList grid of
        [p] -> pure p
        _   -> Left "Unclear start"
    pure $ Track start end grid

-- | Calculates the distance to the end for every tile we can stand on.
distanceToEnd :: Track -> M.Map G.Pos Int
distanceToEnd (Track _ end grid) = Bfs.distances $ Bfs.bfs Bfs.defaultOptions
    { Bfs.start      = S.singleton end
    , Bfs.neighbours = \p -> do
        q <- G.neighbours p
        case G.lookup q grid of
            Just Wall -> []
            Just _    -> [q]
            Nothing   -> []
    }

cheatsStartingFrom :: Track -> M.Map G.Pos Int -> Int -> G.Pos -> [Int]
cheatsStartingFrom (Track _ _ grid) dToEnd limit start = filter (> 0) $ do
    initial <- maybeToList $ M.lookup start dToEnd
    (end, len) <- M.toList shortcuts
    left <- maybeToList $ M.lookup end dToEnd
    pure $ initial - left - len
  where
    shortcuts = Bfs.distances $ Bfs.bfs Bfs.defaultOptions
        { Bfs.start      = S.singleton start
        , Bfs.limit      = Just limit
        , Bfs.neighbours = \p -> do
            q <- G.neighbours p
            case G.lookup q grid of
                Just _ -> [q]
                _      -> []
        }

main :: IO ()
main = pureMain $ \str -> do
    track <- parseTrack str

    let dToEnd = M.insert (tEnd track) 0 $ distanceToEnd track
        part1 = length $ filter (>= 100) $ do
            (start, _) <- M.toList dToEnd
            cheatsStartingFrom track dToEnd 2 start
        part2 = length $ filter (>= 100) $ do
            (start, _) <- M.toList dToEnd
            cheatsStartingFrom track dToEnd 20 start

    pure (pure part1, pure part2)
