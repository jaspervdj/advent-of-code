import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     (many, (<|>))
import           Data.List               (foldl')
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)

data Tile = Empty | Robot | Box | LBox | RBox | Wall deriving (Eq, Show)

data Input = Input
    { inputGrid     :: G.Grid Tile
    , inputCommands :: [G.Dir]
    } deriving (Show)

parseInput :: String -> Either String Input
parseInput input = Input
    <$> (traverse parseTile $ G.fromString (unlines pre))
    <*> NP.runParser (many (NP.spaces *> parseDir)) (unlines post)
  where
    (pre, post) = break null (lines input)
    parseDir =
        (G.U <$ NP.char '^') <|>
        (G.R <$ NP.char '>') <|>
        (G.D <$ NP.char 'v') <|>
        (G.L <$ NP.char '<')

    parseTile '@' = pure Robot
    parseTile '.' = pure Empty
    parseTile 'O' = pure Box
    parseTile '#' = pure Wall
    parseTile c   = Left $ "Unknown char: " ++ show c

widen :: G.Grid Tile -> Either String (G.Grid Tile)
widen grid0 = do
    grid1 <- traverse expand $ grid0
    pure $ M.fromList $ do
        (V2 x y, (l, r)) <- M.toList grid1
        [(V2 (x * 2) y, l), (V2 (x * 2 + 1) y, r)]
  where
    expand Wall  = pure (Wall, Wall)
    expand Empty = pure (Empty, Empty)
    expand Robot = pure (Robot, Empty)
    expand Box   = pure (LBox, RBox)
    expand LBox  = Left "cannot expand LBox"
    expand RBox  = Left "cannot expand RBox"

move :: G.Dir -> G.Pos -> G.Grid Tile -> Maybe (G.Pos, G.Grid Tile)
move dir p grid0 = do
    ptile <- M.lookup p grid0
    qtile <- M.lookup q grid0
    grid1 <- case qtile of
        Wall -> Nothing
        Empty -> Just grid0
        Robot -> Just grid0
        Box -> snd <$> move dir q grid0
        LBox -> do
            grid1 <- snd <$> move dir q grid0
            case dir of
                G.R -> pure grid1  -- Avoid double move
                _   -> snd <$> move dir (G.move 1 G.R q) grid1
        RBox -> do
            grid1 <- snd <$> move dir q grid0
            case dir of
                G.L -> pure grid1  -- Avoid double move
                _   -> snd <$> move dir (G.move 1 G.L q) grid1
    pure (q, M.insert p Empty $ M.insert q ptile grid1)
  where
    q = G.move 1 dir p

coordinates :: G.Grid Tile -> Int
coordinates grid = sum [x + 100 * y | (V2 x y, b) <- M.toList grid, isBox b]
  where
    isBox Box  = True
    isBox LBox = True
    isBox _    = False

simulate :: [G.Dir] -> G.Grid Tile -> Either String (G.Grid Tile)
simulate commands grid0 = do
    robot <- case map fst $ M.toList $ M.filter (== Robot) grid0 of
        [p] -> pure p
        _   -> Left "initial robot position unclear"
    pure $ snd $ foldl'
        (\(p0, g0) dir -> fromMaybe (p0, g0) $ move dir p0 g0)
        (robot, grid0)
        commands

main :: IO ()
main = pureMain $ \str -> do
    Input grid0 commands <- parseInput str
    let part1 = simulate commands grid0
        part2 = widen grid0 >>= simulate commands
    pure (coordinates <$> part1, coordinates <$> part2)
