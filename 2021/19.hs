module Main where

import           AdventOfCode.Main       (pureMain)
import           AdventOfCode.NanoParser as P
import           AdventOfCode.V3         (V3 (..), manhattan, (.+.), (.-.))
import           Control.Monad           (guard)
import           Data.Foldable           (foldl')
import           Data.List.Extra         (select)
import           Data.Maybe              (listToMaybe, maybeToList)
import qualified Data.Set                as S
import           Debug.Trace             (traceM)

data Transform a = RotX | RotY | RotZ | Translate (V3 a) deriving (Show)

applyTransform :: Num a => Transform a -> V3 a -> V3 a
applyTransform RotX          (V3 x y z) = V3 x (-z) y
applyTransform RotY          (V3 x y z) = V3 z y (-x)
applyTransform RotZ          (V3 x y z) = V3 (-y) x z
applyTransform (Translate o) v          = v .+. o

type Transforms a = [Transform a]

applyTransforms :: Num a => Transforms a -> V3 a -> V3 a
applyTransforms trans z = foldl' (\acc r -> applyTransform r acc) z trans

orientations :: [Transforms a]
orientations =
    vars RotZ []                 ++
    vars RotX [RotY]             ++
    vars RotZ [RotY, RotY]       ++
    vars RotX [RotY, RotY, RotY] ++
    vars RotY [RotX]             ++
    vars RotY [RotX, RotX, RotX]
  where
    vars axis b = [b, b ++ [axis], b ++ [axis, axis], b ++ [axis, axis, axis]]

data Scanner = Scanner Int (S.Set (V3 Int))

parseScan :: P.Parser Char Scanner
parseScan = Scanner
    <$> (P.string "--- scanner " *> P.decimal <*
            P.spaces <* P.string "---" <* P.spaces)
    <*> (S.fromList <$> P.sepBy1 parseV3 P.spaces)
  where
    parseV3 = V3
        <$> (P.signedDecimal <* P.char ',')
        <*> (P.signedDecimal <* P.char ',')
        <*> P.signedDecimal

scannerToScan :: Scanner -> Scan
scannerToScan (Scanner _ beacons) = Scan beacons . S.singleton $ V3 0 0 0

data Scan = Scan
    { scanBeacons  :: S.Set (V3 Int)
    , scanScanners :: S.Set (V3 Int)
    } deriving (Show)

-- | Try to merge two scans.  The result will use the perspective of the first
-- scan.
merge :: Scan-> Scan-> Maybe Scan
merge (Scan lbeacons lcams) (Scan rbeacons rcams) = listToMaybe $ do
    lbeacon <- S.toList lbeacons
    rbeacon <- S.toList rbeacons
    orient  <- orientations
    let rbeacon1  = applyTransforms orient rbeacon
        transform = orient ++ [Translate $ lbeacon .-. rbeacon1]
        rbeacons1 = S.map (applyTransforms transform) rbeacons
    guard $ S.size (S.intersection lbeacons rbeacons1) >= 12
    pure $ Scan
        (S.union lbeacons rbeacons1)
        (S.union lcams $ S.map (applyTransforms transform) rcams)

-- | Try to merge a list of scanners
merges :: [Scan] -> Maybe Scan
merges [x] = Just x
merges xs  = step xs >>= merges
  where
    step scanners = listToMaybe $ do
        traceM $ show (length scanners) ++ " scanners left..."
        (s0, scanners')  <- select scanners
        (s1, scanners'') <- select scanners'
        s <- maybeToList $ merge s0 s1
        pure $ s : scanners''

main :: IO ()
main = pureMain $ \input -> do
    scanners <- P.runParser (P.sepBy1 parseScan P.spaces) input
    Scan bs ss <- maybe (Left "Unable to merge all scanners") Right $
        merges (scannerToScan <$> scanners)
    let part1 = S.size bs
        part2 = maximum [manhattan c1 c2 | c1 <- S.toList ss, c2 <- S.toList ss]
    pure (pure part1, pure part2)
