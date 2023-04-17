import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (simpleMain)
import           AdventOfCode.V2     (V2 (..))
import qualified AdventOfCode.V2.Box as Box
import           Control.Monad       (guard)
import           Data.Char           (isDigit, isSpace)
import           Data.List           (foldl', minimumBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, mapMaybe, maybeToList)
import           Data.Ord            (comparing)

data Input = Input (G.Grid Tile) [Command]

data Tile = Floor | Wall deriving (Show)

data Command = Forward Int | TurnLeft | TurnRight deriving (Show)

parseInput :: String -> Input
parseInput str = Input grid commands
  where
    (pre, post) = break null (lines str)

    grid        = M.mapMaybe parseTile . G.fromString $ unlines pre
    parseTile c = case c of
        '#' -> Just Wall
        '.' -> Just Floor
        _   -> Nothing

    commands = parseCommands . filter (not . isSpace) $ concat post
    parseCommands []        = []
    parseCommands ('L' : t) = TurnLeft  : parseCommands t
    parseCommands ('R' : t) = TurnRight : parseCommands t
    parseCommands ls        = case break (not . isDigit) ls of
        (num, t) -> Forward (read num) : parseCommands t

startPosition :: G.Grid Tile -> G.Pos
startPosition = minimumBy (comparing $ \(V2 x y) -> (y, x)) . M.keys

-- | The new heading if you walk (off the edge) in a certain direction.
type Wrapping = M.Map (G.Pos, G.Dir) (G.Pos, G.Dir)

-- | This is the solution to part 1 of the problem.  Wrap around the edges of
-- the map.
simpleWrapping :: G.Grid a -> Wrapping
simpleWrapping grid = M.fromList $ do
    pos0@(V2 x y) <- M.keys grid
    dir           <- [minBound .. maxBound]
    guard . not $ M.member (G.move 1 dir pos0) grid
    let pos1 = case dir of
            G.U -> find dir $ V2 x     bottom
            G.L -> find dir $ V2 right y
            G.D -> find dir $ V2 x     top
            G.R -> find dir $ V2 left  y
    pure ((pos0, dir), (pos1, dir))
  where
    box = fromMaybe (error "empty grid") $ G.box grid

    V2 left top     = Box.bTopLeft     box
    V2 right bottom = Box.bBottomRight box

    find dir pos = case M.lookup pos grid of
        _ | not (Box.inside pos box) ->
            error $ "outside box: " ++ show (dir, pos)
        Nothing -> find dir (G.move 1 dir pos)
        Just _  -> pos

data State = State G.Pos G.Dir deriving (Show)

command :: G.Grid Tile -> Wrapping -> Command -> State -> State
command _ _ TurnLeft    (State pos dir)   = State pos (G.turnLeft dir)
command _ _ TurnRight   (State pos dir)   = State pos (G.turnRight dir)
command _ _ (Forward 0) s                 = s
command g w (Forward n) (State pos0 dir0) = case g M.! pos1 of
    Floor -> command g w (Forward (n - 1)) (State pos1 dir1)
    Wall  -> State pos0 dir0
  where
    try           = G.move 1 dir0 pos0
    (pos1, dir1) = case M.lookup try g of
        Just _  -> (try, dir0)
        Nothing -> w M.! (pos0, dir0)

password :: State -> Int
password (State (V2 x y) dir) =
    1000 * (y + 1) + 4 * (x + 1) + facing
  where
    facing = case dir of
        G.U -> 3
        G.L -> 2
        G.D -> 1
        G.R -> 0

-- | Position where each tile represents an entire face.
newtype FacePos = FacePos (V2 Int) deriving (Eq, Ord, Show)

-- | A face of the cube.  Its position and box of actual coordinates in the
-- input map.
type Faces = M.Map FacePos (Box.Box Int)

-- | All positions on a single edge of a face.  Positions are iterated in a
-- left-to-right position if you were facing towards the edge.
faceEdge :: G.Dir -> Box.Box Int -> [G.Pos]
faceEdge edge box = case edge of
    G.U -> [V2 x    minY | x <- [minX .. maxX]]
    G.R -> [V2 maxX y    | y <- [minY .. maxY]]
    G.D -> [V2 x    maxY | x <- [maxX, maxX - 1 .. minX]]
    G.L -> [V2 minX y    | y <- [maxY, maxY - 1 .. minY]]
  where
    V2 minX minY = Box.bTopLeft     box
    V2 maxX maxY = Box.bBottomRight box

-- | Find a list of faces.  There should be six of them.
faceSearch :: G.Grid a -> Faces
faceSearch grid = M.fromList $ do
    (ix, x) <- zip [0 ..] [minX, minX + side .. maxX]
    (iy, y) <- zip [0 ..] [minY, minY + side .. maxY]
    _       <- maybeToList $ M.lookup (V2 x y) grid
    let faceBox = Box.Box (V2 x y) (V2 (x + side - 1) (y + side - 1))
    pure (FacePos $ V2 ix iy, faceBox)
  where
    side    = minimum $ map width [minY .. maxY]
    width y = length $ mapMaybe (\x -> M.lookup (V2 x y) grid) [minX .. maxX]

    V2 minX minY = Box.bTopLeft     box
    V2 maxX maxY = Box.bBottomRight box

    box = fromMaybe (error "faceSearch: empty") $ G.box grid

-- | Our constraints: when we leave a face in a certain direction, we will end
-- up on a new face, facing another direction.
type Constraints = M.Map (FacePos, G.Dir) (FacePos, G.Dir)

-- | Initial constraints, purely based on face positions.  This is equivalent to
-- the places where faces touch each other on the map.  Walking of the side of
-- the cube this way does not change your direction (from the perspective of the
-- original input).
initConstraints :: [FacePos] -> Constraints
initConstraints positions = M.unions $ do
    FacePos p0 <- positions
    dir0       <- [minBound .. maxBound]
    let p1 = G.move 1 dir0 p0
    guard $ FacePos p1 `elem` positions
    pure $ M.singleton (FacePos p0, dir0) (FacePos p1, dir0)

solveConstraints :: Constraints -> Constraints
solveConstraints constraints0
    | null newConstraints = constraints0
    | otherwise           =
        solveConstraints . M.unions $ constraints0 : newConstraints
  where
    newConstraints = do
        ((p, dir), _) <- M.toList constraints0
        rot           <- [G.turnLeft, G.turnRight]
        walk rot p dir

    walk rot p0 dir0 = do
        (p1, dir1) <- maybeToList $ M.lookup (p0, dir0) constraints0
        let dir1' = rot dir1
        (p2, dir2) <- maybeToList $ M.lookup (p1, dir1') constraints0
        let dir2' = rot dir2
        case M.lookup (p2, dir2') constraints0 of
            Nothing -> [M.singleton (p2, dir2') (p0, rot $ rot $ rot dir0)]
            _       -> []

facesToWrapping :: Faces -> Constraints -> Wrapping
facesToWrapping faces constraints = M.fromList $ do
    (fpos0, fbox0) <- M.toList faces
    dir0           <- [minBound .. maxBound]
    (fpos1, dir1)  <- maybeToList $ M.lookup (fpos0, dir0) constraints
    fbox1          <- maybeToList $ M.lookup fpos1 faces
    let pos0     = faceEdge dir0 fbox0
        lookback = G.turnAround dir1
        pos1     = reverse $ faceEdge lookback fbox1
    zipWith (\p0 p1 -> ((p0, dir0), (p1, dir1))) pos0 pos1

main :: IO ()
main = simpleMain $ \str ->
    let Input grid commands = parseInput str

        state0 = State (startPosition grid) G.R

        solve wrapping = password $ foldl'
            (\acc move -> command grid wrapping move acc)
            state0 commands

        wrapping1 = simpleWrapping grid

        faces       = faceSearch grid
        constraints = solveConstraints . initConstraints $ M.keys faces
        wrapping2   = facesToWrapping faces constraints in

    (solve wrapping1, solve wrapping2)
