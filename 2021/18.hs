{-# LANGUAGE LambdaCase #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import Debug.Trace
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))

data T = I Int | P T T

instance Show T where
    show = \case
        I n -> show n
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
data Step = Explode Int Int (Zipper T) | Split Int (Zipper T) deriving (Show)

next :: T -> Maybe Step
next = go (0 :: Int) []
  where
    go depth z node = case node of
        P (I x) (I y) | depth >= 4 -> Just $ Explode x y z
        P x y                      -> go (depth + 1) (Left  y : z) x <|>
                                      go (depth + 1) (Right x : z) y
        I n | n >= 10              -> Just $ Split n z
        I _                        -> Nothing

step :: Step -> T
step (Explode n k z) =
    build (Just n) (Just k) (I 0) z
  where
    build _   _   t [] = t
    build mbX mbY t (Left r : zs) = case mbY of
        Nothing -> build mbX Nothing (P t r)          zs
        Just y  -> build mbX Nothing (P t (addL y r)) zs
    build mbX mbY t (Right l : zs) = case mbX of
        Nothing -> build Nothing mbY (P l          t) zs
        Just x  -> build Nothing mbY (P (addR x l) t) zs
step (Split n z) =
    let x = n `div` 2
        y = n - x in
    build (P (I x) (I y)) z
  where
    build t []             = t
    build t (Left  r : zs) = build (P t r) zs
    build t (Right l : zs) = build (P l t) zs

reduce :: T -> T
reduce t = case next t of
    Nothing -> t
    Just s  -> reduce (step s)

solve :: T -> [String]
solve t = case next t of
    Nothing -> [show t]
    Just s  -> show t : solve (step s)

main :: IO ()
main = pureMain $ \input -> do
    ts <- P.runParser (P.sepBy1 parseT P.spaces) input
    let sum1 = foldl1 (\acc x -> P acc x) ts
    traceM $ unlines $ solve sum1
    pure (pure (show sum1), pure "k")
