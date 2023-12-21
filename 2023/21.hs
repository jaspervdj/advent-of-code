import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..))
import qualified AdventOfCode.Z3           as Z3
import           Control.Monad             (guard)
import           Data.Maybe                (maybeToList)
import qualified Data.Set                  as S

data Tile = Garden | Rock | Start deriving (Eq, Show)

parseTile :: Char -> Either String Tile
parseTile 'S' = pure Start
parseTile '.' = pure Garden
parseTile '#' = pure Rock
parseTile c   = Left $ "bad tile: " ++ show c

step :: G.Grid Tile -> S.Set G.Pos -> S.Set G.Pos
step grid positions = S.fromList $ do
    pos <- S.toList positions
    next <- G.neighbours pos
    tile <- maybeToList $ G.lookup next grid
    guard $ tile /= Rock
    pure next

data Pos2 = Pos2 !G.Pos !G.Pos deriving (Eq, Ord)

wrap :: G.Grid a -> Pos2 -> Pos2
wrap g pos2@(Pos2 (V2 x y) tile)
    | x < 0               = Pos2 (V2 (G.gridWidth g - 1) y)  (G.move 1 G.L tile)
    | x >= G.gridWidth g  = Pos2 (V2 0 y)                    (G.move 1 G.R tile)
    | y < 0               = Pos2 (V2 x (G.gridHeight g - 1)) (G.move 1 G.U tile)
    | y >= G.gridHeight g = Pos2 (V2 x 0)                    (G.move 1 G.D tile)
    | otherwise           = pos2

step2 :: G.Grid Tile -> S.Set Pos2 -> S.Set Pos2
step2 grid positions = S.fromList $ do
    Pos2 p0 t0 <- S.toList positions
    p1 <- G.neighbours p0
    let Pos2 p2 t1 = wrap grid (Pos2 p1 t0)
    tile <- maybeToList $ G.lookup p2 grid
    guard $ tile /= Rock
    pure (Pos2 p2 t1)

solveQuadratic :: [(Int, Int)] -> Int -> Z3.Program
solveQuadratic pts goal =
    Z3.declareConst "a" Z3.RealType <>
    Z3.declareConst "b" Z3.RealType <>
    Z3.declareConst "c" Z3.RealType <>
    mconcat [Z3.declareConstEq var Z3.RealType (Z3.int x) | (var, x) <- xs] <>
    mconcat [Z3.declareConstEq var Z3.RealType (Z3.int y) | (var, y) <- ys] <>
    Z3.declareConstEq "x" Z3.RealType (Z3.int goal) <>
    mconcat [
        Z3.assert $ q x Z3..= Z3.var y
    | (x, y) <- zip (map fst xs) (map fst ys)] <>
    Z3.checkSat <>
    Z3.eval (Z3.toInt (q "x"))
  where
    xs = [("x" <> show i, x) | (i, x) <- zip [0 :: Int ..] $ map fst pts]
    ys = [("y" <> show i, y) | (i, y) <- zip [0 :: Int ..] $ map snd pts]
    q x = (Z3..+)
        [ (Z3..*) [Z3.var "a", Z3.var x, Z3.var x]
        , (Z3..*) [Z3.var "b", Z3.var x]
        , Z3.var "c"
        ]

main :: IO ()
main = ioMain $ \str -> do
    grid <- either fail pure $ G.fromString str >>= traverse parseTile
    let starts1 = S.fromList [pos | (pos, ty) <- G.toList grid, ty == Start]
        part1   = (!! 64) $ iterate (step grid) starts1
        starts2 = S.map (\x -> Pos2 x (V2 0 0)) starts1
    period <- if G.gridWidth grid == G.gridHeight grid
        then pure $ G.gridWidth grid
        else fail "unequal width/height"
    let goal = 26501365
        remainder = goal `mod` period
        points   =
            take 3 $
            filter ((== remainder) . (`mod` period) . fst) $
            zip [0 :: Int ..] $ map S.size $
            iterate (step2 grid) starts2
        part2 = Z3.run "2023-21" $ solveQuadratic points goal
    pure (pure (S.size part1), part2)
