-- | I was mainly trying to roll an Applicative-only parser.  Since then I moved
-- it to the 'AdventOfCode.NanoParser' module.
{-# LANGUAGE DeriveFunctor #-}

import           AdventOfCode.NanoParser
import           Control.Applicative     (Alternative (..))

data Group
    = Group [Group]
    | Garbage String
    deriving (Show)

group :: Parser Char Group
group =
    (Garbage <$> (char '<' *> garbage)) <|>
    (Group   <$> (char '{' *> commaSep group <* char '}'))
  where
    garbage =
        (char '!' *> anyChar *> garbage) <|>
        (char '>' *> pure []) <|>
        ((:) <$> anyChar <*> garbage)

    commaSep p = sepBy p (char ',')

score :: Group -> Int
score = go 1
  where
    go level (Group stream) = level + sum (map (go (level + 1)) stream)
    go _     (Garbage _)    = 0

count :: Group -> Int
count (Garbage str)  = length str
count (Group stream) = sum (map count stream)

main :: IO ()
main = do
    input <- getContents
    case runParser group input of
        Left err -> fail $ "Parse failed: " ++ err
        Right x  -> do
            putStrLn $ "Score: " ++ show (score x)
            putStrLn $ "Garbage count: " ++ show (count x)
