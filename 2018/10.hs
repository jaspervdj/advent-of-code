{-# LANGUAGE BangPatterns #-}

import qualified AdventOfCode.Grid   as G
import           AdventOfCode.V2
import qualified AdventOfCode.V2.Box as Box
import           Data.Char           (isDigit)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Semigroup      ((<>))
import qualified System.IO           as IO

data Light = Light
    { lPos :: !(V2 Int)
    , lVel :: !(V2 Int)
    } deriving (Show)

updateLight :: Light -> Light
updateLight l = l {lPos = lPos l .+. lVel l}

parseLights :: IO.Handle -> IO [Light]
parseLights h =
    IO.hGetContents h >>= mapM parseLight . lines
  where
    parseLight line = case map read (words (map space line)) of
        [px, py, vx, vy] -> return $ Light (V2 px py) (V2 vx vy)
        _                -> fail $ "Could not parse line: " ++ show line

    space c = if isDigit c || c == '-' then c else ' '

smallest :: [Light] -> (Int, [Light])
smallest =
    \lights -> go 0 (area lights) lights
  where
    area = Box.area . L.foldl1' (<>) . map (Box.fromV2 . lPos)

    go !time area0 lights0 =
        let lights1 = map updateLight lights0
            area1   = area lights1 in
        if area1 > area0 then (time, lights0) else go (time + 1) area1 lights1

main :: IO ()
main = do
    lights <- parseLights IO.stdin
    let (time, solution) = smallest lights
        grid             = M.fromList [(lPos l, '#') | l <- solution]
    G.printGrid IO.stdout grid
    print time

