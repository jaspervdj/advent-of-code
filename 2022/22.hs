import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (simpleMain)
import           AdventOfCode.V2     (V2 (..))
import qualified AdventOfCode.V2.Box as Box
import           Data.Char           (isDigit, isSpace)
import           Data.List           (foldl', minimumBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (comparing)

data Input = Input
    { inputGrid  :: GridBox
    , inputMoves :: [Move]
    }

data GridBox = GridBox (G.Grid Tile) (Box.Box Int)

data Tile = Floor | Wall deriving (Show)

data Move = Forward Int | TurnLeft | TurnRight deriving (Show)

parseInput :: String -> Input
parseInput str =
    Input (GridBox grid box) moves
  where
    (pre, post) = break null (lines str)

    grid        = M.mapMaybe parseTile . G.fromString $ unlines pre
    parseTile c = case c of
        '#' -> Just Wall
        '.' -> Just Floor
        _   -> Nothing

    moves = parseMoves . filter (not . isSpace) $ concat post
    parseMoves []        = []
    parseMoves ('L' : t) = TurnLeft  : parseMoves t
    parseMoves ('R' : t) = TurnRight : parseMoves t
    parseMoves ls        = case break (not . isDigit) ls of
        (num, t) -> Forward (read num) : parseMoves t

    box = fromMaybe (error "empty grid") $ G.box grid

startPosition :: G.Grid Tile -> G.Pos
startPosition = minimumBy (comparing $ \(V2 x y) -> (y, x)) . M.keys

data Walker = Walker G.Pos G.Dir deriving (Show)

wrap :: GridBox -> G.Dir -> G.Pos -> G.Pos
wrap (GridBox grid box) dir (V2 x y) = case dir of
    G.U -> find $ V2 x     bottom
    G.L -> find $ V2 right y
    G.D -> find $ V2 x     top
    G.R -> find $ V2 left  y
  where
    V2 left top     = Box.bTopLeft     box
    V2 right bottom = Box.bBottomRight box

    find pos = case M.lookup pos grid of
        _ | not (Box.inside pos box) -> error $ "outside box: " ++ show (dir, pos)
        Nothing -> find (G.move 1 dir pos)
        Just _  -> pos


moveWalker :: GridBox -> Move -> Walker -> Walker

moveWalker _ TurnLeft    (Walker pos dir) = Walker pos (G.turnLeft dir)
moveWalker _ TurnRight   (Walker pos dir) = Walker pos (G.turnRight dir)

moveWalker _                   (Forward 0) walker           = walker
moveWalker gb@(GridBox grid _) (Forward n) (Walker pos dir) =
    case M.lookup next1 grid of
        Just Floor -> moveWalker gb (Forward (n - 1)) (Walker next1 dir)
        Just Wall  -> Walker pos dir
        Nothing    -> error "internal error: not wrapped"
  where
    next0 = G.move 1 dir pos
    next1 = case M.lookup next0 grid of
        Just _  -> next0
        Nothing -> wrap gb dir pos


walkerPassword :: Walker -> Int
walkerPassword (Walker (V2 x y) dir) =
    1000 * (y + 1) + 4 * (x + 1) + facing
  where
    facing = case dir of
        G.U -> 3
        G.L -> 2
        G.D -> 1
        G.R -> 0


main :: IO ()
main = simpleMain $ \str ->
    let input          = parseInput str
        GridBox grid _ = inputGrid input

        walker0 = Walker (startPosition grid) G.R
        part1   = walkerPassword $ foldl'
            (\acc move -> moveWalker (inputGrid input) move acc)
            walker0 (inputMoves input) in
    (part1, "bar")
