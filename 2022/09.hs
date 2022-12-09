{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..), (.+.), (.-.))
import qualified AdventOfCode.V2         as V2
import           Control.Applicative     (many, (<|>))
import           Data.List               (foldl')
import qualified Data.Set                as S
import qualified Data.Vector             as V

type Input = [(G.Dir, Int)]

parseInput :: NP.Parser Char Input
parseInput = many $ (,) <$> dir <* NP.char ' ' <*> NP.decimal <* NP.newline
  where
    dir =
        G.U <$ NP.char 'U' <|>
        G.R <$ NP.char 'R' <|>
        G.D <$ NP.char 'D' <|>
        G.L <$ NP.char 'L'

drag :: G.Pos -> G.Pos -> G.Pos
drag h t
    | abs dx <= 1 && abs dy <= 1 = t
    | otherwise                  = t .+. V2 (signum dx) (signum dy)
  where
    V2 dx dy = h .-. t

type Rope = V.Vector G.Pos

step :: G.Dir -> Rope -> Rope
step dir rope = V.unfoldrExactN
    (V.length rope)
    (\(i, lead) ->
        let p = if i <= 0 then G.move 1 dir lead else drag lead (rope V.! i) in
        (p, (i + 1, p)))
    (0, V.head rope)

solve :: Rope -> [G.Dir] -> Int
solve rope0 = S.size . snd . foldl'
    (\(!rope, !acc) dir ->
        let rope' = step dir rope
            acc' = S.insert (V.last rope') acc in
        (rope', acc'))
    (rope0, S.singleton (V.last rope0))

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseInput str
    let moves = input >>= \(dir, n) -> replicate n dir
        part1 = solve (V.replicate  2 V2.zero) moves
        part2 = solve (V.replicate 10 V2.zero) moves
    pure (pure part1, pure part2)
