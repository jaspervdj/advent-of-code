{-# LANGUAGE RecordWildCards #-}
module Main where

import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as P
import           AdventOfCode.V2         (V2 (..), (.+.))
import           AdventOfCode.V2.Box     (Box (..), fromV2, inside)
import           Control.Monad           (guard)
import           Data.Foldable           (toList)

type Target = Box Int

parseTarget :: P.Parser Char Target
parseTarget = mkTarget
    <$> (P.string "target area:" *> P.spaces *> P.string "x=" *> range)
    <*> (P.char ',' *> P.spaces *> P.string "y=" *> range)
  where
    range = (,) <$> P.signedDecimal <* P.string ".." <*> P.signedDecimal
    mkTarget (x0, x1) (y0, y1) = fromV2 (V2 x0 y0) <> fromV2 (V2 x1 y1)

data Probe = Probe
    { probePos :: !(V2 Int)
    , probeVel :: !(V2 Int)
    } deriving (Show)

step :: Probe -> Probe
step Probe {..} = Probe
    { probePos = probePos .+. probeVel
    , probeVel = V2
        { vX = case vX probeVel of
            x | x < 0     -> x + 1
              | x > 0     -> x - 1
              | otherwise -> x
        , vY = vY probeVel - 1
        }
    }

launch :: V2 Int -> [Probe]
launch = iterate step . Probe (V2 0 0)

overshot :: Target -> Probe -> Bool
overshot target Probe {..} =
    (vX probePos > vX (bBottomRight target) && vX probeVel > 0) ||
    (vY probePos < vY (bTopLeft target)     && vY probeVel < 0)

main :: IO ()
main = pureMain $ \input -> do
    target <- P.runParser parseTarget input
    let guess = maximum (map abs $ toList target) * 2
        hits = do
            vel <- V2 <$> [0 .. guess] <*> [-guess .. guess]
            let path = takeWhile (not . overshot target) $ launch vel
            guard $ any (`inside` target) (map probePos path)
            pure path
        highest = maximum $ map (vY . probePos) $ concat hits
    pure (pure highest, pure (length hits))
