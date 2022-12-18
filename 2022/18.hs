import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V3         (V3 (..))
import qualified Data.Map                as M

parseInput :: NP.Parser Char [V3 Int]
parseInput = v3 `NP.sepBy1` NP.newline
  where
    v3 = V3
        <$> NP.signedDecimal
        <*> (NP.char ',' *> NP.signedDecimal)
        <*> (NP.char ',' *> NP.signedDecimal)

part1 :: [V3 Int] -> Int
part1 cubes = M.size . M.filter (== 1) $
    M.fromListWith (+) [(p, 1 :: Int) | c <- cubes, p <- sideCenters c]
  where
    sideCenters (V3 x y z) =
        [ V3 (x * 2 + 1) (y * 2 + 1) (z * 2    )
        , V3 (x * 2    ) (y * 2 + 1) (z * 2 + 1)
        , V3 (x * 2 + 1) (y * 2)     (z * 2 + 1)
        , V3 (x * 2 + 1) (y * 2 + 1) (z * 2 + 2)
        , V3 (x * 2 + 2) (y * 2 + 1) (z * 2 + 1)
        , V3 (x * 2 + 1) (y * 2 + 2) (z * 2 + 1)
        ]

main :: IO ()
main = pureMain $ \input -> do
    cubes <- NP.runParser parseInput input
    pure (pure (part1 cubes), pure "bar")
