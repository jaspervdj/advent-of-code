{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main
import           AdventOfCode.V2     (V2 (..))
import           AdventOfCode.V2.Box (Box (..))
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)
import qualified Data.Set            as S

type Flow = G.Dir -> [G.Dir]

charToFlow :: Char -> Flow
charToFlow '.' dir = [dir]
charToFlow '/' dir = case dir of
    G.U -> [G.R]
    G.R -> [G.U]
    G.D -> [G.L]
    G.L -> [G.D]
charToFlow '\\' dir = case dir of
    G.U -> [G.L]
    G.R -> [G.D]
    G.D -> [G.R]
    G.L -> [G.U]
charToFlow '-' dir = case dir of
    G.U -> [G.R, G.L]
    G.D -> [G.R, G.L]
    _   -> [dir]
charToFlow '|' dir = case dir of
    G.R -> [G.U, G.D]
    G.L -> [G.U, G.D]
    _   -> [dir]
charToFlow _ _ = []

beam :: G.Grid Flow -> G.Pos -> G.Dir -> G.Grid (S.Set G.Dir)
beam grid pos0 dir0 = go M.empty [(pos0, dir0)]
  where
    go !acc [] = acc
    go !acc ((pos, dir) : queue)
        | Just visited <- M.lookup pos acc, dir `S.member` visited =
            go acc queue
        | otherwise = case M.lookup pos grid of
            Nothing -> go acc queue
            Just flow -> go
                (M.insertWith S.union pos (S.singleton dir) acc)
                ([(G.move 1 dir' pos, dir') | dir' <- flow dir] <> queue)

main :: IO ()
main = simpleMain $ \str ->
    let grid  = charToFlow <$> G.fromString str
        part1 = beam grid (V2 0 0) G.R

        (Box (V2 minX minY) (V2 maxX maxY)) = fromJust $ G.box grid
        part2 = maximum . map (M.size . uncurry (beam grid)) $
            [(V2 x    0,    G.D) | x <- [minX .. maxX]] ++
            [(V2 maxX y,    G.L) | y <- [minY .. maxY]] ++
            [(V2 x    maxY, G.U) | x <- [minX .. maxX]] ++
            [(V2 0    y,    G.R) | y <- [minY .. maxY]] in
    (M.size part1, part2)
