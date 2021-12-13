{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           Data.Char               (isLower, isUpper)
import           Data.Foldable           (foldl')
import qualified Data.Map                as M
import qualified Data.Set                as S

isBig :: String -> Bool
isBig = \case x : _ -> isUpper x; _ -> False

isSmall :: String -> Bool
isSmall = \case s@(x : _) -> isLower x && s /= "start" && s /= "end"; _ -> False

type Graph a = M.Map a (S.Set a)

insertEdge :: Ord a => (a, a) -> Graph a -> Graph a
insertEdge (x, y) =
    M.insertWith S.union x (S.singleton y) .
    M.insertWith S.union y (S.singleton x)

parseGraph :: Ord a => P.Parser Char a -> P.Parser Char (Graph a)
parseGraph p =
    foldl' (flip insertEdge) M.empty <$> P.many1 (parseEdge <* P.spaces)
  where
    parseEdge = (,) <$> p <* P.char '-' <*> p

paths :: Ord a => (a -> Bool) -> (a -> Bool) -> a -> a -> Graph a -> [[a]]
paths visitMany visitTwice start end graph =
    go []   S.empty False start where
    go path visited twice pos
        | pos == end = [reverse (end : path)]
        | otherwise  = do
            next <- maybe [] S.toList $ M.lookup pos graph
            if  | visitMany next ->
                    go (pos : path) (S.insert pos visited) twice next
                | S.member next visited && visitTwice next && not twice ->
                    go (pos : path) (S.insert pos visited) True next
                | S.member next visited ->
                    []
                | otherwise ->
                    go (pos : path) (S.insert pos visited) twice next

main :: IO ()
main = pureMain $ \input -> do
    caves <- P.runParser (parseGraph (P.many1 P.alpha)) input
    let part1 = length $ paths isBig (const False) "start" "end" caves
        part2 = length $ paths isBig isSmall "start" "end" caves
    pure (pure part1, pure part2)
