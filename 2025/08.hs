import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V3         (V3 (..))
import           Control.Applicative     (many)
import           Data.List               (sort, sortOn)
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Vector             as V

parsePoints :: NP.Parser Char [V3 Int]
parsePoints = many $ V3
    <$> (NP.decimal <* NP.char ',')
    <*> (NP.decimal <* NP.char ',')
    <*> (NP.decimal <* NP.spaces)

squaredDistance :: V3 Int -> V3 Int -> Int
squaredDistance (V3 x0 y0 z0) (V3 x1 y1 z1) =
    (x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1) + (z0 - z1) * (z0 - z1)

closestPairs :: V.Vector (V3 Int) -> [(V3 Int, V3 Int)]
closestPairs points = map snd $ sortOn fst $ do
    i <- [0 .. V.length points - 2]
    j <- [i + 1 .. V.length points - 1]
    let p = points V.! i
        q = points V.! j
    pure (squaredDistance p q, (p, q))

-- | Connect the pairs in order.  Returns the groups of connected junctions as
-- well as the last connection made.
connect :: Ord a => [(a, a)] -> ([[a]], Maybe (a, a))
connect pairs = (groups, lastConnected)
  where
    (roots, lastConnected) = build M.empty Nothing pairs
    groups = map (S.toList . snd) . M.toList $ M.fromListWith (<>)
        [(root p roots, S.singleton p) | (l, r) <- pairs, p <- [l, r]]

    build acc lc [] = (acc, lc)
    build acc lc ((p, q) : ps)
        | pr == qr  = build acc lc ps
        | otherwise = build (M.insert pr qr acc) (Just (p, q)) ps
      where
        pr = root p acc
        qr = root q acc

    -- Could shorten these on lookup to improve performance.
    root p m = maybe p (`root` m) (M.lookup p m)

main :: IO ()
main = pureMain $ \str -> do
    points <- NP.runParser parsePoints str
    let part1 = product $ take 3 $ reverse $ sort $ map length $ fst $ connect $
            take 1000 $ closestPairs $ V.fromList points
        part2 = case snd $ connect $ closestPairs $ V.fromList points of
            Nothing     -> Left "no connection"
            Just (p, q) -> pure (v3X p * v3X q)
    pure (pure part1, part2)
