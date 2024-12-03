import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Data.List               (foldl')

parseMul :: NP.Parser Char (Int, Int)
parseMul = (,)
    <$> (NP.string "mul(" *> NP.decimal <* NP.char ',')
    <*> (NP.decimal <* NP.char ')')

data Instr = Mul (Int, Int) | Do | Don't | Crap Char deriving (Show)

parseInstrs :: NP.Parser Char [Instr]
parseInstrs = many $
    Mul   <$> parseMul            <|>
    Don't <$  NP.string "don't()" <|>
    Do    <$  NP.string "do()"    <|>
    Crap  <$> NP.anyChar

main :: IO ()
main = pureMain $ \input -> do
    instrs <- NP.runParser parseInstrs input
    let part1 = sum [x * y | Mul (x, y) <- instrs]
        (part2, _) = foldl' (\(!acc, !enabled) instr -> case instr of
            Mul (x, y) -> (acc + if enabled then x * y else 0, enabled)
            Crap _     -> (acc, enabled)
            Do         -> (acc, True)
            Don't      -> (acc, False)) (0, True) instrs
    pure (pure part1, pure part2)
