module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           AdventOfCode.V2         (V2 (..))
import           Data.Foldable           (toList)
import           Data.List               (partition)
import qualified Data.Map                as M
import qualified Data.Set                as S

parseInput :: P.Parser Char ([Int], [G.Grid Int])
parseInput = (,)
    <$> (sep P.decimal (P.char ',') <* eol <* eol)
    <*> (sep parseBoard eol)
  where
    sep p s        = toList <$> P.sepBy1 p s
    eol            = P.horizontalSpaces <* P.newline
    parseBoard     = G.fromList <$> sep parseBoardLine eol <* eol
    parseBoardLine = P.horizontalSpaces *> sep P.decimal P.horizontalSpaces

data Line = Row !Int | Col !Int deriving (Eq, Ord)

wins :: Ord a => S.Set a -> G.Grid a -> Bool
wins drawn = or . M.foldrWithKey (\(V2 x y) val acc ->
    let ok = val `S.member` drawn in
    M.insertWith (&&) (Row y) ok $ M.insertWith (&&) (Col x) ok acc) M.empty

score :: (Num a, Ord a) => S.Set a -> G.Grid a -> a
score drawn = sum . filter (not . (`S.member` drawn)) . map snd . M.toList

-- Returns an chronological list of the scores of the winners.
play :: [Int] -> [G.Grid Int] -> [Int]
play = go S.empty
  where
    go _     []            _      = []
    go drawn (n : numbers) boards =
        [score drawn' w * n | w <- winners] ++ go drawn' numbers boards'
      where
        drawn'             = S.insert n drawn
        (winners, boards') = partition (wins drawn') boards

main :: IO ()
main = pureMain $ \inputStr -> do
    (numbers, boards) <- P.runParser parseInput inputStr
    let winners = play numbers boards
    pure (pure (head winners), pure (last winners))
