import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Data.List               (transpose)

parseOp :: NP.Parser Char ([Int] -> Int)
parseOp = (NP.char '*' *> pure product) <|> (NP.char '+' *> pure sum)

part1 :: String -> Either String Int
part1 str = do
    (nums, ops) <- NP.runParser problem str
    pure $ sum $ zipWith ($) ops (transpose nums)
  where
    problem = (,) <$> many (line NP.decimal <* NP.newline) <*> line parseOp
    line p  = NP.horizontalSpaces *> many (p <* NP.horizontalSpaces)

part2 :: String -> Either String Int
part2 str = do
    problems <- NP.runParser (many problem) $ concat $
        transpose $ map reverse $ lines str
    pure $ sum [op nums | (nums, op) <- problems]
  where
    problem = (,)
        <$> (NP.spaces *> many (NP.decimal <* NP.spaces))
        <*> (parseOp <* NP.spaces)

main :: IO ()
main = pureMain $ \str -> pure (part1 str, part2 str)
