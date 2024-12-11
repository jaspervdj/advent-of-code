import           AdventOfCode.Main   (simpleMain)
import           Control.Monad.State (MonadState, evalState, gets, modify)
import qualified Data.Map            as M

-- Avoid mixing up all those ints.
newtype Steps = Steps Int     deriving (Eq, Ord)
newtype Stone = Stone Int     deriving (Eq, Ord)
newtype Size  = Size  Integer deriving (Eq, Ord, Num)

blink :: Stone -> [Stone]
blink (Stone n)
    | n == 0    = [Stone 1]
    | odd len   = [Stone (n * 2024)]
    | otherwise = map Stone [read l, read r]
  where
    str    = show n
    len    = length str
    (l, r) = splitAt (len `div` 2) str

cached :: (MonadState (M.Map k v) m, Ord k) => k -> m v -> m v
cached k f = do
    mbCached <- gets (M.lookup k)
    case mbCached of
        Just c  -> pure c
        Nothing -> do
            r <- f
            modify (M.insert k r)
            pure r

cachedBlink
    :: MonadState (M.Map (Steps, Stone) Size) m
    => Steps -> [Stone] -> m Size
cachedBlink (Steps 0) = pure . Size . fromIntegral . length
cachedBlink (Steps s) = fmap sum . traverse
    (\n -> cached (Steps s, n) $ cachedBlink (Steps (s - 1)) $ blink n)

main :: IO ()
main = simpleMain $ \input ->
    let stones     = map (Stone . read) (words input)
        part1      = length $ iterate (>>= blink) stones !! 25
        Size part2 = evalState (cachedBlink (Steps 75) stones) M.empty in
    (part1, part2)
