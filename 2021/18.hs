{-# LANGUAGE LambdaCase #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))

data T = I Int | P T T deriving (Eq)

instance Show T where
    show = \case
        I n   -> show n
        P x y -> "[" ++ show x ++ "," ++ show y ++ "]"

addL, addR :: Int -> T -> T
addL x (I n)   = I (x + n)
addL x (P l r) = P (addL x l) r
addR x (I n)   = I (x + n)
addR x (P l r) = P l (addR x r)

magnitude :: T -> Int
magnitude (I n)   = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r

parseT :: P.Parser Char T
parseT =
    (I <$> P.decimal) <|>
    (P <$> (P.char '[' *> parseT <* P.char ',') <*> (parseT <* P.char ']'))

type Zipper a = [Either a a]

findExplode :: T -> Maybe (Int, Int, Zipper T)
findExplode = go (0 :: Int) []
  where
    go depth z node = case node of
        P (I x) (I y) | depth >= 4 -> Just (x, y, z)
        P x y                      -> go (depth + 1) (Left  y : z) x <|>
                                      go (depth + 1) (Right x : z) y
        I _                        -> Nothing

doExplode :: (Int, Int, Zipper T) -> T
doExplode (n, k, z) =
    go (Just n) (Just k) (I 0) z
  where
    go _   _   t [] = t
    go mbX mbY t (Left r : zs) = case mbY of
        Nothing -> go mbX Nothing (P t r)          zs
        Just y  -> go mbX Nothing (P t (addL y r)) zs
    go mbX mbY t (Right l : zs) = case mbX of
        Nothing -> go Nothing mbY (P l          t) zs
        Just x  -> go Nothing mbY (P (addR x l) t) zs

findSplit :: T -> Maybe (Int, Zipper T)
findSplit = go []
  where
    go z node = case node of
        P x y -> go (Left  y : z) x <|> go (Right x : z) y
        I n   -> if n >= 10 then Just (n, z) else Nothing

doSplit :: (Int, Zipper T) -> T
doSplit (n, z) =
    let x = n `div` 2
        y = n - x in
    go (P (I x) (I y)) z
  where
    go t []             = t
    go t (Left  r : zs) = go (P t r) zs
    go t (Right l : zs) = go (P l t) zs

reduce :: T -> T
reduce t = case findExplode t of
    Just expl -> reduce $ doExplode expl
    Nothing -> case findSplit t of
        Just s  -> reduce $ doSplit s
        Nothing -> t

add :: T -> T -> T
add x y = reduce $ P x y

main :: IO ()
main = pureMain $ \input -> do
    ts <- P.runParser (P.sepBy1 parseT P.spaces) input
    let part1 = foldl1 add ts
        part2 = maximum [magnitude $ add x y | x <- ts, y <- ts, x /= y]
    pure (pure (magnitude part1), pure part2)
