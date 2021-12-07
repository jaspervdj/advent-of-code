-- | Okay so this one is pretty ugly.
import           Control.Monad               (foldM, forM)
import           Control.Monad.Extra         (ifM)
import           Control.Monad.Primitive     (PrimMonad (..))
import           Control.Monad.ST            (runST)
import           Data.Char                   (digitToInt, intToDigit)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Scoreboard s a = Scoreboard
    { sScores :: !(VUM.MVector s a)
    , sSize   :: !Int
    , sElf1   :: !Int
    , sElf2   :: !Int
    }

initial :: PrimMonad m => m (Scoreboard (PrimState m) Int)
initial = do
    scores <- VUM.new 32
    VUM.write scores 0 3
    VUM.write scores 1 7
    return $ Scoreboard scores 2 0 1

-- | Add a recipe to the end.
add :: (PrimMonad m, VUM.Unbox a)
    => Scoreboard (PrimState m) a -> a -> m (Scoreboard (PrimState m) a)
add s x = do
    scores <- if sSize s >= VUM.length (sScores s)
                then VUM.grow (sScores s) (sSize s)
                else return (sScores s)
    VUM.write scores (sSize s) x
    return s {sScores = scores, sSize = sSize s + 1}

-- | Combine the two elf's recipes and add them.
step
    :: PrimMonad m
    => Scoreboard (PrimState m) Int -> m (Scoreboard (PrimState m) Int)
step s = do
    x <- VUM.read (sScores s) (sElf1 s)
    y <- VUM.read (sScores s) (sElf2 s)
    let digits = map digitToInt $ show (x + y)
    s' <- foldM add s digits
    return s'
        { sElf1 = (sElf1 s + 1 + x) `mod` sSize s'
        , sElf2 = (sElf2 s + 1 + y) `mod` sSize s'
        }

-- | Check if the scoreboard matches the list at a given offset
matches
    :: (PrimMonad m, Eq a, VU.Unbox a)
    => Scoreboard (PrimState m) a -> Int -> VU.Vector a -> m Bool
matches s offset vec
    | offset < 0 || offset + VU.length vec >= sSize s = return False
    | otherwise                                       = go 0
  where
    go i
        | i >= VU.length vec = return True
        | otherwise          = do
            x <- VUM.read (sScores s) (offset + i)
            if x == vec VU.! i then go (i + 1) else return False

generateRecipes :: PrimMonad m => Int -> m (Scoreboard (PrimState m) Int)
generateRecipes n = initial >>= go
  where
    go s = if sSize s < n then step s >>= go else return s

generateUntilPattern :: PrimMonad m => [Int] -> m Int
generateUntilPattern pattern = initial >>= go
  where
    vec = VU.fromList pattern
    go s =
        -- `step` can add two digits so we test at two indices.
        let idx1 = sSize s - VU.length vec
            idx2 = idx1 - 1 in
        ifM (matches s idx1 vec)
            (return idx1)
            (ifM (matches s idx2 vec) (return idx2) (step s >>= go))

main :: IO ()
main = do
    input <- getContents
    let n = read input
    s <- generateRecipes (n + 10)
    scores <- forM [n .. n + 9] $ \i -> VUM.read (sScores s) i
    putStrLn $ map intToDigit scores
    print $ runST $ generateUntilPattern $ map digitToInt input
