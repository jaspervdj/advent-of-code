import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V3         (V3 (..), (.-.))
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.List               (sortOn)
import           Data.List.Extra         (select)
import qualified Data.Set                as S

type Brick = S.Set (V3 Int)

parseBricks :: NP.Parser Char [Brick]
parseBricks = fmap toList $ NP.many1 $
    makeBrick <$> (parseV3 <* NP.char '~') <*> (parseV3 <* NP.spaces)
  where
    parseV3 = V3
        <$> (NP.decimal <* NP.char ',')
        <*> (NP.decimal <* NP.char ',')
        <*> NP.decimal

    makeBrick (V3 px py pz) (V3 qx qy qz)
        | px /= qx  = S.fromList [V3 x  py pz | x <- [px .. qx]]
        | py /= qy  = S.fromList [V3 px  y pz | y <- [py .. qy]]
        | pz /= qz  = S.fromList [V3 px py  z | z <- [pz .. qz]]
        | otherwise = S.singleton $ V3 px py pz

-- Drops all bricks; returns the new bricks (not necessarily in the same order)
-- and the number of bricks that moved.
fall :: [Brick] -> ([Brick], Int)
fall =
    go 0 mempty [] . zip (repeat 0) . sortOn (minimum . fmap v3Z . S.toList)
  where
    go nfallen _     dropped [] = (dropped, nfallen)
    go nfallen stack dropped ((fell, b0) : bs)
        | any ((<= 1) . v3Z) b0 || any (`S.member` stack) b1 =
            go (nfallen + fell) (stack <> b0) (b0 : dropped) bs
        | otherwise = go nfallen stack dropped ((1, b1) : bs)
      where
        b1 = S.map (.-. V3 0 0 1) b0

main :: IO ()
main = pureMain $ \str -> do
    bricks <- NP.runParser parseBricks str
    let (stable, _) = fall bricks
        part1 = length $ do
            (brick, remainder) <- select stable
            guard $ snd (fall remainder) == 0
            pure brick
        part2 = sum $ do
            (_, remainder) <- select stable
            pure . snd $ fall remainder
    pure (pure part1, pure part2)
