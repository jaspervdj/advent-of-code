{-# LANGUAGE BangPatterns #-}

import           Data.List     (foldl')
import           Data.Monoid   ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Spinlock = Spinlock
    { sPosition :: !Int
    , sBuffer   :: !(Seq Int)
    } deriving (Show)

zero :: Spinlock
zero = Spinlock
    { sPosition = 0
    , sBuffer   = Seq.singleton 0
    }

stepInsert :: Int -> Int -> Spinlock -> Spinlock
stepInsert !step !value !s =
    let !pos      = (sPosition s + step) `mod` Seq.length (sBuffer s) + 1
        !(!l, !r) = Seq.splitAt pos (sBuffer s) in
    s {sBuffer = l <> Seq.singleton value <> r, sPosition = pos}

-- | Value after the current position in the buffer
nextValue :: Spinlock -> Int
nextValue (Spinlock p b) = Seq.index b (p + 1)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    step <- readLn
    let to2017 = foldl' (\s n -> stepInsert step n s) zero [1 .. 2017]
    putStrLn $ "After 2017: " ++ show (nextValue to2017)

    let amount = 50000000 :: Int
        fbig   = foldl' (\s n -> fastStepInsert step n s) fastZero [1 .. amount]
    putStrLn $ "After 0 (fast): " ++ show (fsSecond fbig)

    let big    = foldl' (\s n -> stepInsert step n s) zero [1 .. amount]
        Just idx0 = Seq.findIndexL (== 0) (sBuffer big)
        value     = Seq.index (sBuffer big) (idx0 + 1)
    putStrLn $ "After 0 (brute force): " ++ show value

--------------------------------------------------------------------------------

data FastSpinlock = FastSpinlock
    { fsPosition :: !Int
    , fsSize     :: !Int
    , fsSecond   :: !Int
    } deriving (Show)

fastZero :: FastSpinlock
fastZero = FastSpinlock 0 1 0

fastStepInsert :: Int -> Int -> FastSpinlock -> FastSpinlock
fastStepInsert !step !value !fs =
    let !pos = (fsPosition fs + step) `mod` (fsSize fs) + 1 in
    FastSpinlock
        { fsPosition = pos
        , fsSize     = fsSize fs + 1
        , fsSecond   = if pos == 1 then value else fsSecond fs
        }
