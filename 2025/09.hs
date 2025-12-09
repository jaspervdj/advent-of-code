import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..), (.*), (.+.), (.-.))
import           AdventOfCode.V2.Box     (Box (..))
import qualified AdventOfCode.V2.Box     as Box
import           Control.Applicative     (many)
import           Data.List               (sortOn)
import           Data.Ord                (Down (..))
import qualified Data.Vector             as V

parsePoints :: NP.Parser Char (V.Vector (V2 Int))
parsePoints = fmap V.fromList . many $ V2
    <$> (NP.decimal <* NP.char ',')
    <*> (NP.decimal <* NP.spaces)

biggestAreas :: V.Vector (V2 Int) -> [(Int, (V2 Int, V2 Int))]
biggestAreas points = sortOn (Down . fst) $ do
    i <- [0 .. V.length points - 2]
    j <- [i + 1 .. V.length points - 1]
    let p        = points V.! i
        q        = points V.! j
        V2 dx dy = p .-. q
    pure ((abs dx + 1) * (abs dy + 1), (p, q))

-- | Uses the shoelace formula to determine if the points are ordered in
-- clockwise fashion.  Alternatively I could hardcode if my input is
-- clockwise...
clockwise :: V.Vector (V2 Int) -> Bool
clockwise points = (> 0) $ sum $ do
    i <- [0 .. V.length points - 1]
    let j        = (i + 1) `mod` (V.length points)
        V2 x0 y0 = points V.! i
        V2 x1 y1 = points V.! j
    pure $ x0 * y1 - x1 * y0

-- | Converts a list of corner tiles that form a (completely axis aligned, not
-- self intersecting) polygon to the exact lines that form the polygon (going
-- on the outside of all the tiles).
toExact :: V.Vector (V2 Int) -> Either String (V.Vector (V2 Int))
toExact points0 = traverse point $ V.zip3
    (V.drop (len - 1) points <> V.take (len - 1) points)
    points
    (V.drop 1 points <> V.take 1 points)
  where
    points = if clockwise points0 then points0 else V.reverse points0
    len    = V.length points

    point corner@(V2 prevX prevY, p@(V2 x y), V2 nextX nextY)
        | x > prevX, nextY > y = pure $ p .+. V2 1 0  -- Right, down
        | x > prevX, nextY < y = pure $ p .+. V2 0 0  -- Right, up
        | y > prevY, nextX > x = pure $ p .+. V2 1 0  -- Down, right
        | y > prevY, nextX < x = pure $ p .+. V2 1 1  -- Down, left
        | x < prevX, nextY > y = pure $ p .+. V2 1 1  -- Left, down
        | x < prevX, nextY < y = pure $ p .+. V2 0 1  -- Left, up
        | y < prevY, nextX > x = pure $ p .+. V2 0 0  -- Up, right
        | y < prevY, nextX < x = pure $ p .+. V2 0 1  -- Up, left
        | otherwise            =
            Left $ "invalid corner: " ++ show corner

toBoxes :: V.Vector (V2 Int) -> V.Vector (Box Int)
toBoxes points = V.zipWith toBox points (V.drop 1 points <> V.take 1 points)
  where
    toBox p q = Box.fromV2 p <> Box.fromV2 q

main :: IO ()
main = pureMain $ \str -> do
    points <- NP.runParser parsePoints str
    let part1 = case biggestAreas points of
            []           -> Left "no areas"
            ((a, _) : _) -> pure a
        part2 = do
            boxes <- toBoxes . fmap (.* 2) <$> toExact points
            let fits (p, q) =
                    let box = Box.fromV2 (p .* 2 .+. V2 1 1) <>
                              Box.fromV2 (q .* 2 .+. V2 1 1) in
                    V.all (not . Box.collides box) boxes
            case filter (fits . snd) (biggestAreas points) of
                []           -> Left "no areas"
                ((a, _) : _) -> pure a
    pure (part1, part2)
