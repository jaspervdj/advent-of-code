module Main where

import qualified AdventOfCode.BinarySearch as BS
import qualified AdventOfCode.Grid         as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser   as NP
import qualified AdventOfCode.V2           as V2
import           Control.Monad             (guard)
import           Data.Functor              (($>))
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe, listToMaybe, mapMaybe)
import qualified System.IO                 as IO

-- Try to fit the ship somewhere based on its top Y coordinate.
fitShipAtY :: (Int -> Int -> Bool) -> Int -> Int -> Maybe G.Pos
fitShipAtY beam size top = do
    beamLeft <- L.find (\x -> beam x top) [0 .. top * 10]
    beamWidth <- succ <$> BS.upperBound
        (\w -> guard (beam (beamLeft + w) top) $> w)
    guard $ beamWidth >= size
    let left   = beamLeft + beamWidth - size
        bottom = top + size - 1
    guard $ beam left bottom
    pure (V2.V2 left top)

main :: IO ()
main = do
    program <- NP.hRunParser IO.stdin parseProgram
    let size = 100
        beam = \x y -> case evalMachine (initMachine [x, y] program) of
            1 : _ -> True
            _     -> False

    print . sum $ concatMap evalMachine
        [initMachine [x, y] program | y <- [0 .. 49], x <- [0 .. 49]]

    print . fromMaybe 0 $ do
        -- The binary search may not give us the best solution since it's not
        -- continuous, so we need to try some other positions around there as
        -- well.
        guess     <- fmap V2.v2Y $ BS.lowerBound $ \y -> fitShipAtY beam size y
        V2.V2 x y <- listToMaybe $ mapMaybe (fitShipAtY beam 100)
            [guess - size .. guess]
        pure $ x * 10000 + y
