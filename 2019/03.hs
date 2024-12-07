module Main where

import qualified AdventOfCode.Grid       as G
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Foldable           (toList)
import           Data.Functor            (($>))
import           Data.List               (scanl')
import qualified Data.Map                as Map
import           Data.Tuple              (swap)
import qualified System.IO               as IO

type Wire = [(G.Dir, Int)]

parseWires :: NP.Parser Char [Wire]
parseWires = fmap toList $ NP.many1 $
    NP.sepBy1 ((,) <$> parseDir <*> NP.decimal) (NP.char ',') <* NP.spaces
  where
    parseDir =
        (NP.char 'U' $> G.U) <|>
        (NP.char 'R' $> G.R) <|>
        (NP.char 'D' $> G.D) <|>
        (NP.char 'L' $> G.L)

positions :: Wire -> [G.Pos]
positions =
    drop 1 . scanl' (flip (G.move 1)) G.origin .
    concatMap (uncurry replicate . swap)

main :: IO ()
main = do
    wires <- NP.hRunParser IO.stdin parseWires
    let [m1, m2] = map (Map.fromList . flip zip [1 ..] . positions) wires
        crosses  = Map.intersectionWith (+) m1 m2 :: Map.Map G.Pos Int
    print . minimum . map (G.manhattan G.origin . fst) $ Map.toList crosses
    print . minimum . map snd $ Map.toList crosses
