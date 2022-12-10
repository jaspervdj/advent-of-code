{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     (many, (<|>))
import qualified Data.Map                as M
import qualified Data.Set                as S

data Instruction = Noop | AddX Int deriving (Show)

parseInstructions :: NP.Parser Char [Instruction]
parseInstructions = many $ parseInstruction <* NP.newline
  where
    parseInstruction =
        Noop <$ NP.string "noop" <|>
        (AddX <$> (NP.string "addx" *> NP.horizontalSpaces *> NP.signedDecimal))

part1 :: [Instruction] -> Int
part1 = go 0 1 1
  where
    go !acc _ _ [] = acc
    go !acc pc x (Noop : instrs) = go (acc + signal pc x) (pc + 1) x instrs
    go !acc pc x (AddX n : instrs) = go
        (acc + signal pc x + signal (pc + 1) x)
        (pc + 2)
        (x + n)
        instrs

    signal pc x = if pc `mod` 40 == 20 then pc * x else 0

part2 :: [Instruction] -> S.Set G.Pos
part2 = go S.empty 0 1
  where
    go !grid _  _      []                = grid
    go !grid pc sprite (Noop   : instrs) =
        go (draw pc sprite grid) (pc + 1) sprite instrs
    go !grid pc sprite (AddX n : instrs) = go
        (draw (pc + 1) sprite . draw pc sprite $ grid)
        (pc + 2)
        (sprite + n)
        instrs

    draw pc sprite grid
        | col < sprite - 1 || col > sprite + 1 = grid
        | otherwise                            = S.insert (V2 col row) grid
      where
        (row, col) = pc `divMod` 40

main :: IO ()
main = pureMain $ \ input -> do
    instructions <- NP.runParser parseInstructions input
    let crt = M.fromSet (const '#') $ part2 instructions
    pure (pure (part1 instructions), pure (G.toString crt))
