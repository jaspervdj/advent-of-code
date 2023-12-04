import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main
import           AdventOfCode.V2   (V2 (..))
import           Control.Monad     (guard)
import           Data.Char         (isDigit)
import qualified Data.Map          as M

data Number = Number
    { numberPosition :: G.Pos
    , numberValue    :: Int
    , numberWidth    :: Int
    } deriving (Show)

findNumbers :: G.Grid Char -> [Number]
findNumbers grid = map mkNumber positions
  where
    positions = do
        (pos, c) <- M.toList grid
        guard $ isDigit c
        guard $ maybe True (not . isDigit) $ M.lookup (G.move 1 G.L pos) grid
        pure pos

    pspan =
        takeWhile (maybe False isDigit . flip M.lookup grid) .
        iterate (G.move 1 G.R)

    mkNumber pos =
        let str = map (grid M.!) (pspan pos) in
        Number pos (read str) (length str)

numberNeighbours :: Number -> [G.Pos]
numberNeighbours (Number (V2 px py) _ w) =
    [V2 x (py - 1) | x <- [px - 1 .. px + w]] ++
    [V2 (px - 1) py, V2 (px + w) py] ++
    [V2 x (py + 1) | x <- [px - 1 .. px + w]]

main :: IO ()
main = simpleMain $ \string ->
    let grid = G.fromString string
        numbers = findNumbers grid

        symbol c = not $ isDigit c || c == '.'
        part = any (maybe False symbol . flip M.lookup grid) . numberNeighbours
        part1 = sum $ map numberValue $ filter part numbers

        adjacenct = M.fromListWith (++) $
            [(nb, [n]) | n <- numbers, nb <- numberNeighbours n]
        part2 = sum $
            [ numberValue n1 * numberValue n2
            | (p, [n1, n2]) <- M.toList adjacenct
            , M.lookup p grid == Just '*'
            ] in

    (part1, part2)
