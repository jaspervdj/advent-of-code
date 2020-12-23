import           AdventOfCode.Main           (simpleMain)
import           Control.Monad               (replicateM_, zipWithM_)
import           Control.Monad.ST            (ST, runST)
import           Data.Char                   (digitToInt, intToDigit, isDigit)
import qualified Data.Vector.Unboxed.Mutable as VUM

type Cups s = VUM.MVector s Int

cupsFromList :: [Int] -> ST s (Cups s)
cupsFromList numbers = do
    cups <- VUM.new (length numbers + 1)  -- Slow?
    zipWithM_ (VUM.write cups) numbers (drop 1 numbers <> numbers)
    VUM.write cups 0 (head numbers)
    pure cups

cupsToList :: Cups s -> ST s [Int]
cupsToList cups = do
    start <- VUM.read cups 0
    let go x = do
            y <- VUM.read cups x
            if y == start then pure [x] else (x :) <$> go y
    VUM.read cups 0 >>= go

moveCups :: Cups s -> ST s ()
moveCups cups = do
    currentCup <- VUM.read cups 0
    picked1 <- VUM.read cups currentCup
    picked2 <- VUM.read cups picked1
    picked3 <- VUM.read cups picked2
    afterPicked <- VUM.read cups picked3
    VUM.write cups currentCup afterPicked

    let findLabel n
            | n <= 0 = findLabel (VUM.length cups - 1)
            | n == picked1 || n == picked2 || n == picked3 = findLabel (n - 1)
            | otherwise = n
        label = findLabel (currentCup - 1)

    labelNext <- VUM.read cups label
    VUM.write cups label picked1
    VUM.write cups picked3 labelNext

    nextCup <- VUM.read cups currentCup
    VUM.write cups 0 nextCup

main :: IO ()
main = simpleMain $ \inputstr ->
    let numbers = map digitToInt $ filter isDigit inputstr in
    ( runST $ do
        cups <- cupsFromList numbers
        replicateM_ 100 $ moveCups cups
        list <- cupsToList cups
        let (pre, post) = break (== 1) list
        pure $ map intToDigit $ drop 1 post <> pre
    , runST $ do
        cups <- cupsFromList $ numbers ++ [maximum numbers + 1 .. 1000000]
        replicateM_ 10000000 $ moveCups cups
        x <- VUM.read cups 1
        y <- VUM.read cups x
        pure $ x * y
    )
