{-# LANGUAGE RecordWildCards #-}
import qualified AdventOfCode.Grid.Bounded as G
import qualified AdventOfCode.Loop as Loop
import           AdventOfCode.Main
import           AdventOfCode.V2           (V2 (..))
import           Data.List                 (sortOn, foldl')
import qualified Data.Set                  as S

data Platform = Platform
    { pGrid  :: !(G.Grid Bool)
    , pRocks :: !(S.Set G.Pos)
    } deriving (Show)

platformFromString :: String -> Either String Platform
platformFromString str = do
    grid <- G.fromString str
    let static = fmap (== '#') grid
        rocks  = map fst . filter ((== 'O') . snd) $ G.toList grid
    pure $ Platform static (S.fromList rocks)

platformToString :: Platform -> String
platformToString Platform {..} = G.toString $ G.mapWithKey toChar pGrid
  where
    toChar pos cube
        | cube                  = '#'
        | pos `S.member` pRocks = 'O'
        | otherwise             = '.'

-- Sort rocks in the order we want to move them, based on the direction.
queueRocks :: G.Dir -> Platform -> [G.Pos]
queueRocks dir = sortOn attr . S.toList . pRocks
  where
    attr = case dir of
        G.U -> v2Y
        G.R -> negate . v2X
        G.D -> negate . v2Y
        G.L -> v2X

-- Roll a single rock into the given direction.
rollRock :: G.Dir -> G.Pos -> Platform -> Platform
rollRock dir pos0 platform@Platform {..} =
    platform {pRocks = S.insert (go pos0) $ S.delete pos0 pRocks}
  where
    go pos = case G.lookup next pGrid of
        Just False | not (next `S.member` pRocks) -> go next
        _                                         -> pos
      where
        next = G.move 1 dir pos

-- Roll all rocks.
rollRocks :: G.Dir -> Platform -> Platform
rollRocks dir platform =
    foldl' (\a p -> rollRock dir p a) platform $ queueRocks dir platform

totalLoad :: Platform -> Int
totalLoad Platform {..} =
    sum [G.gridHeight pGrid - y | V2 _ y <- S.toList pRocks]

main :: IO ()
main = pureMain $ \str -> do
    platform <- platformFromString str
    let part1 = rollRocks G.U platform

    let step = rollRocks G.R . rollRocks G.D . rollRocks G.L . rollRocks G.U
    loop <- maybe (Left "no loop") Right $ Loop.findLoop pRocks step platform
    let smaller = Loop.equivalent loop 1000000000
        part2   = (!! smaller) $ iterate step platform

    pure (pure (totalLoad part1), pure (totalLoad part2))
