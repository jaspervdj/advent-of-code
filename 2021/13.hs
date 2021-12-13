module Main where

import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     ((<|>))
import           Data.Coerce             (coerce)
import qualified Data.Map                as M
import           Data.Monoid             (Dual (..), Endo (..))
import qualified Data.Set                as S

type Fold  = Dual (Endo (V2 Int))
type Sheet = S.Set (V2 Int)

parseInput :: P.Parser Char (Sheet, [Fold])
parseInput = (,) <$> (S.fromList <$> P.many1 v2) <*> P.many1 fold
  where
    v2   = V2 <$> (P.decimal <* P.char ',') <*> (P.decimal <* P.spaces)
    fold = P.string "fold along " *> xy <* P.spaces
    xy   = ((foldX <$ P.string "x=") <|> (foldY <$ P.string "y=")) <*> P.decimal

foldX, foldY :: Int -> Fold
foldX xf = coerce $ \(V2 x y) -> if x > xf then V2 (2 * xf - x) y else V2 x y
foldY yf = coerce $ \(V2 x y) -> if y > yf then V2 x (2 * yf - y) else V2 x y

foldSheet :: Fold -> Sheet -> Sheet
foldSheet = S.map . coerce

sheetToString :: Sheet -> String
sheetToString s = G.toString $ M.fromList [(p, '#') | p <- S.toList s]

main :: IO ()
main = pureMain $ \input -> do
    (sheet, folds) <- P.runParser parseInput input
    let part1 = foldSheet (head folds)    sheet
        part2 = foldSheet (mconcat folds) sheet
    pure (pure (S.size part1), pure (sheetToString part2))
