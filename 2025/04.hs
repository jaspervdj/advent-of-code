import qualified AdventOfCode.Grid as G
import           AdventOfCode.Main (pureMain)
import qualified Data.Map          as M
import qualified Data.Set          as S

type Rolls = S.Set G.Pos

parseRolls :: String -> Either String Rolls
parseRolls =
    fmap (M.keysSet . M.filter id) . traverse parseTile . G.fromString
  where
    parseTile '@' = Right True
    parseTile '.' = Right False
    parseTile c   = Left $ "unknown char: " ++ show c

accessible :: Rolls -> G.Pos -> Bool
accessible grid p = (< 4) . length $
    [q | q <- G.neighbours p ++ G.diagonal p, q `S.member` grid]

cleanup :: Rolls -> Rolls
cleanup grid
    | S.null removable = grid
    | otherwise        = cleanup (grid `S.difference` removable)
  where
    removable = S.filter (accessible grid) grid

main :: IO ()
main = pureMain $ \str -> do
    grid <- parseRolls str
    let part1 = length $ [p | p <- S.toList grid, accessible grid p]
        part2 = S.size grid - S.size (cleanup grid)
    pure (pure part1, pure part2)
