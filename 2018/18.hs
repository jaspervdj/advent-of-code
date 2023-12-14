{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid as G
import qualified AdventOfCode.Loop as Loop
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe        (mapMaybe)
import qualified System.IO         as IO

data Acre = Ground | Tree | Lumberyard deriving (Eq, Ord)

parseAcre :: Char -> IO Acre
parseAcre '.' = return Ground
parseAcre '|' = return Tree
parseAcre '#' = return Lumberyard
parseAcre c   = fail $ "Bad acre: " ++ show c

showAcre :: Acre -> Char
showAcre Ground     = '.'
showAcre Tree       = '|'
showAcre Lumberyard = '#'

type Landscape = G.Grid Acre

stepAcre :: Acre -> [Acre] -> Acre
stepAcre Ground adj
    | length (filter (== Tree) adj) >= 3 = Tree
    | otherwise                          = Ground
stepAcre Tree adj
    | length (filter (== Lumberyard) adj) >= 3 = Lumberyard
    | otherwise                                = Tree
stepAcre Lumberyard adj
    | Lumberyard `elem` adj && Tree `elem` adj = Lumberyard
    | otherwise                                = Ground

stepLandscape :: Landscape -> Landscape
stepLandscape landscape = M.mapWithKey
    (\pos acre ->
        let poss = G.neighbours pos ++ G.diagonal pos
            adjacent = mapMaybe (`M.lookup` landscape) poss in
        stepAcre acre adjacent)
    landscape

resourceValue :: Landscape -> Int
resourceValue landscape =
    let (tree, lumber) = L.foldl' count (0, 0) landscape in tree * lumber
  where
    count (!tree, !lumber) Tree       = (tree + 1, lumber)
    count (!tree, !lumber) Lumberyard = (tree, lumber + 1)
    count acc              Ground     = acc

main :: IO ()
main = do
    grid <- G.readGrid parseAcre IO.stdin

    -- Part 1
    let states = iterate stepLandscape grid
    G.printGrid IO.stderr $ fmap showAcre $ states !! 10
    print $ resourceValue $ states !! 10

    -- Part 2
    let loop = Loop.findLoop id stepLandscape grid
        idx  = Loop.equivalent loop 1000000000
    print $ resourceValue $ states !! idx
