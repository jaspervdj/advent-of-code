module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     ((<|>))
import           Data.Foldable           (foldl')
import qualified Data.Map                as M
import qualified Data.Set                as S

data Fold = X Int | Y Int deriving (Show)
type Sheet = S.Set (V2 Int)

parseInput :: P.Parser Char (Sheet, [Fold])
parseInput = (,) <$> (S.fromList <$> P.many1 v2) <*> P.many1 fold
  where
    v2   = V2 <$> (P.decimal <* P.char ',') <*> (P.decimal <* P.spaces)
    fold = P.string "fold along " *> xy <* P.spaces
    xy   = ((X <$ P.string "x=") <|> (Y <$ P.string "y=")) <*> P.decimal

foldPoint :: Fold -> V2 Int -> V2 Int
foldPoint (X xf) (V2 x y) = if x > xf then V2 (2 * xf - x) y else V2 x y
foldPoint (Y yf) (V2 x y) = if y > yf then V2 x (2 * yf - y) else V2 x y

foldSheet :: Fold -> Sheet -> Sheet
foldSheet f = S.map (foldPoint f)

sheetToString :: Sheet -> String
sheetToString s = G.toString $ M.fromList [(p, '#') | p <- S.toList s]

main :: IO ()
main = pureMain $ \input -> do
    (sheet, folds) <- P.runParser parseInput input
    let part1 = foldSheet (head folds) sheet
        part2 = foldl' (flip foldSheet) sheet folds
    pure (pure (S.size part1), pure (sheetToString part2))
