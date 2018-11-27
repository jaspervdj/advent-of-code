{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified AdventOfCode.Hex      as Hex
import qualified Data.ByteString.Char8 as BC8
import           Data.Char             (isSpace)
import           Data.List             (foldl')

-- | Follow a path.  Also return the furthest distance from the start we
-- encountered.
path :: [Hex.Dir] -> Hex.Cubic -> (Hex.Cubic, Int)
path dirs origin = foldl'
    (\(!p0, !maxdist0) dir ->
        let !p1       = Hex.move dir p0
            !maxdist1 = max maxdist0 (Hex.distance p1 origin) in
        (p1, maxdist1))
    (origin, 0)
    dirs

-- | Parse directions
parse :: BC8.ByteString -> [Hex.Dir]
parse input =
    [x | d <- BC8.split ',' (BC8.filter (not . isSpace) input), x <- parseDir d]
  where
    parseDir :: BC8.ByteString -> [Hex.Dir]
    parseDir "n"  = [Hex.N]
    parseDir "ne" = [Hex.NE]
    parseDir "se" = [Hex.SE]
    parseDir "s"  = [Hex.S]
    parseDir "sw" = [Hex.SW]
    parseDir "nw" = [Hex.NW]
    parseDir _    = []

main :: IO ()
main = do
    input <- BC8.getContents
    let (endpos, maxdist) = path (parse input) Hex.zero
    putStrLn $ "Distance to final position: " ++
        show (Hex.distance Hex.zero endpos)
    putStrLn $ "Distance to furthest position: " ++ show maxdist
