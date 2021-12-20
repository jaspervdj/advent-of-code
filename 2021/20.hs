{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase     #-}
module Main where

import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (simpleMain)
import           AdventOfCode.V2     (V2 (..))
import           Data.Foldable       (foldl')
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Vector.Unboxed as VU

type IEA = VU.Vector Bool

data Image = Image Bool (S.Set G.Pos)  -- Background, pixels different from BG

toBool :: Char -> Bool
toBool '.' = False
toBool '#' = True
toBool c   = error $ "Unknown character: " ++ show c

parse :: String -> (IEA, Image)
parse input = (iea, img)
  where
    ls  = lines input
    iea = VU.fromList . map toBool $ head ls
    img = Image False . M.keysSet . M.filter id . M.filter id $
        fmap toBool . G.fromString . unlines $ drop 2 ls

(!) :: Image -> G.Pos -> Bool
Image def s ! p = if p `S.member` s then not def else def

-- Ordered neighbourhood
neighbourhood :: G.Pos -> [G.Pos]
neighbourhood (V2 x y) =
    [ V2 (x - 1) (y - 1), V2 x (y - 1), V2 (x + 1) (y - 1)
    , V2 (x - 1)  y     , V2 x  y     , V2 (x + 1)  y
    , V2 (x - 1) (y + 1), V2 x (y + 1), V2 (x + 1) (y + 1)
    ]

binaryValue :: G.Pos -> Image -> Int
binaryValue pos img = foldl'
    (\acc p -> acc * 2 + if img ! p then 1 else 0) 0 (neighbourhood pos)

step :: IEA -> Image -> Image
step iea img@(Image def grid) = Image def'
    (S.filter (\pos -> def' /= iea VU.! binaryValue pos img) full)
  where
    def' = if def then iea VU.! 0b1111111 else iea VU.! 0
    full = S.fromList $ S.toList grid >>= neighbourhood >>= neighbourhood

main :: IO ()
main = simpleMain $ \input ->
    let (iea, img0) = parse input
        img2        = step iea $ step iea img0
        img50       = iterate (step iea) img0 !! 50
        numLight = \case
            Image True _ -> "infinite"
            Image _    s -> show $ S.size s in
    (numLight img2, numLight img50)
