import           AdventOfCode.Main
import           Control.Monad               (replicateM_)
import           Control.Monad.Primitive     (PrimMonad (..))
import           Control.Monad.ST            (runST)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable as VUM

data RingThing s a = RingThing
    { rtThings :: V.Vector a
    , rtNext   :: VUM.MVector s Int
    , rtPrev   :: VUM.MVector s Int
    }

ringThingIndex :: RingThing s a -> Int -> a
ringThingIndex rt i = rtThings rt V.! i

ringThingIndexOf :: Eq a => RingThing s a -> a -> Maybe Int
ringThingIndexOf rt x = V.findIndex (== x) (rtThings rt)

ringThingFromList :: PrimMonad m => [a] -> m (RingThing (PrimState m) a)
ringThingFromList list =
    let things = V.fromList list
        len    = V.length things in
    RingThing things
        <$> VUM.generate len (\i -> if i + 1 == len then 0 else i + 1)
        <*> VUM.generate len (\i -> if i == 0 then len - 1 else i - 1)

ringThingNext :: PrimMonad m => RingThing (PrimState m) a -> Int -> Int -> m Int
ringThingNext rt idx0 repeats = go idx0 0
  where
    go idx r
        | r >= repeats = pure idx
        | otherwise    = do
            next <- VUM.read (rtNext rt) idx
            go next (r + 1)

ringThingMove :: PrimMonad m => RingThing (PrimState m) a -> Int -> Int -> m ()
ringThingMove rt idx num
    | rightwards < leftwards = replicateM_ rightwards moveRight
    | otherwise              = replicateM_ leftwards moveLeft
  where
    len = V.length $ rtThings rt
    rightwards = num `mod` (len - 1)
    leftwards = len - rightwards - 1

    -- We only need one of these, but having both makes it faster.
    moveLeft = do
        prev <- VUM.read (rtPrev rt) idx
        prevPrev <- VUM.read (rtPrev rt) prev
        next <- VUM.read (rtNext rt) idx
        VUM.write (rtNext rt) prevPrev idx
        VUM.write (rtPrev rt) idx prevPrev
        VUM.write (rtNext rt) idx prev
        VUM.write (rtPrev rt) prev idx
        VUM.write (rtNext rt) prev next
        VUM.write (rtPrev rt) next prev

    moveRight = do
        prev <- VUM.read (rtPrev rt) idx
        next <- VUM.read (rtNext rt) idx
        nextNext <- VUM.read (rtNext rt) next
        VUM.write (rtNext rt) prev next
        VUM.write (rtPrev rt) idx next
        VUM.write (rtNext rt) idx nextNext
        VUM.write (rtPrev rt) next prev
        VUM.write (rtNext rt) next idx
        VUM.write (rtPrev rt) nextNext idx

ringThingMix :: PrimMonad m => RingThing (PrimState m) Int -> m ()
ringThingMix rt = V.iforM_ (rtThings rt) $ \idx num -> ringThingMove rt idx num

ringThingCoordinates :: PrimMonad m => RingThing (PrimState m) Int -> m Int
ringThingCoordinates rt = do
    let Just zeroIdx = ringThingIndexOf rt 0
    idx1000 <- ringThingNext rt zeroIdx 1000
    idx2000 <- ringThingNext rt idx1000 1000
    idx3000 <- ringThingNext rt idx2000 1000
    pure $
        ringThingIndex rt idx1000 +
        ringThingIndex rt idx2000 +
        ringThingIndex rt idx3000

main :: IO ()
main = simpleMain $ \input ->
    let numbers = map read $ lines input :: [Int]
        part1 = runST $ do
            rt <- ringThingFromList numbers
            ringThingMix rt
            ringThingCoordinates rt
        part2 = runST $ do
            rt <- ringThingFromList $ (* 811589153) <$> numbers
            replicateM_ 10 $ ringThingMix rt
            ringThingCoordinates rt in
    (part1, part2)
