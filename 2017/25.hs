{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST, runST)
import           Data.Char                   (chr, ord)
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable                 as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import           System.IO                   as IO

--------------------------------------------------------------------------------

data Tape s a = Tape
    { tZero  :: !a
    , tLeft  :: !(VUM.MVector s a)
    , tRight :: !(VUM.MVector s a)
    }

newTape :: VUM.Unbox a => a -> ST s (Tape s a)
newTape z = Tape z <$> VUM.replicate 64 z <*> VUM.replicate 64 z

readTape :: VUM.Unbox a => Tape s a -> Int -> ST s a
readTape Tape {..} i
    | i < 0  && j < VUM.length tLeft  = VUM.unsafeRead tLeft  j
    | i >= 0 && i < VUM.length tRight = VUM.unsafeRead tRight i
    | otherwise                       = return tZero
  where
    j = -i

writeTape :: forall s a. VUM.Unbox a => Tape s a -> Int -> a -> ST s (Tape s a)
writeTape t i x
    | i < 0 =
        let !j = -i in if j < VUM.length (tLeft t)
            then VUM.unsafeWrite (tLeft t) j x >> return t
            else do
                l <- grow (tLeft t)
                VUM.unsafeWrite l j x
                return t {tLeft = l}
    | otherwise =
        if i < VUM.length (tRight t)
            then VUM.unsafeWrite (tRight t) i x >> return t
            else do
                r <- grow (tRight t)
                VUM.unsafeWrite r i x
                return t {tRight = r}
  where
    grow :: VUM.MVector s a -> ST s (VUM.MVector s a)
    grow v0 = do
        let !len = VUM.length v0
        v1 <- VUM.grow v0 len
        forM_ [len .. len * 2 - 1] $ \idx -> VUM.unsafeWrite v1 idx (tZero t)
        return v1

checksumTape :: forall s a. (Eq a, VUM.Unbox a) => Tape s a -> a -> ST s Int
checksumTape t x = (+) <$> go (tLeft t) 0 0 <*> go (tRight t) 0 0
  where
    go :: VUM.MVector s a -> Int -> Int -> ST s Int
    go v !acc !i
        | i >= VUM.length v = return acc
        | otherwise         = do
            y <- VUM.unsafeRead v i
            go v (if x == y then acc + 1 else acc) (i + 1)

--------------------------------------------------------------------------------

newtype StateName = StateName {unStateName :: Int}

instance Show StateName where
    show (StateName n) = [chr (ord 'A' + n)]

instance Read StateName where
    readsPrec _ (c : cs)
        | c >= 'A' && c <= 'Z' = [(StateName (ord c - ord 'A'), cs)]
    readsPrec _ _              = []

data Blueprint = Blueprint
    { bInitialState  :: !StateName
    , bChecksumAfter :: !Int
    , bStateSpecs    :: !(V.Vector StateSpec)
    }

data StateSpec = StateSpec
    { ssName   :: {-# UNPACK #-} !StateName
    , ssIfZero :: {-# UNPACK #-} !StepSpec
    , ssIfOne  :: {-# UNPACK #-} !StepSpec
    } deriving (Show)

data StepSpec = StepSpec
    { ssWrite :: !Bool
    , ssLeft  :: !Bool
    , ssNext  :: {-# UNPACK #-} !StateName
    } deriving (Show)

paragraphs :: String -> [[String]]
paragraphs str0 = go (lines str0)
  where
    go ls0 = case break null ls0 of
        ([],  _)         -> []
        (par, [] : more) -> par : go more
        (par, _)         -> [par]

readBlueprint :: IO.Handle -> IO Blueprint
readBlueprint h = do
    paras <- map (map words) . paragraphs . clean <$> IO.hGetContents h
    let !bInitialState  = read (paras !! 0 !! 0 !! 3)
        !bChecksumAfter = read (paras !! 0 !! 1 !! 5) :: Int

    stateSpecs <- mapM parseStateSpec (tail paras)
    let !bStateSpecs = V.create $ do
            v <- VM.new 32
            forM_ stateSpecs $ \ss ->
                VM.unsafeWrite v (unStateName (ssName ss)) ss
            return v

    return Blueprint {..}
  where
    clean :: String -> String
    clean = filter (\c -> c /= '.' && c /= ':')

    parseStateSpec :: [[String]] -> IO StateSpec
    parseStateSpec para = StateSpec (read (para !! 0 !! 2))
        <$> parseStepSpec (take 3 (drop 2 para))
        <*> parseStepSpec (take 3 (drop 6 para))

    parseStepSpec :: [[String]] -> IO StepSpec
    parseStepSpec para = return $! StepSpec
        (para !! 0 !! 4 == "1")
        (para !! 1 !! 6 == "left")
        (read (para !! 2 !! 4))

--------------------------------------------------------------------------------

runBlueprint :: Blueprint -> Int
runBlueprint blueprint = runST $ do
    tape0 <- newTape False
    tape1 <- go tape0 0 0 (bInitialState blueprint)
    checksumTape tape1 True
  where
    go tape0 !steps !cursor !(StateName stateName)
        | steps >= bChecksumAfter blueprint = return tape0
        | otherwise                         = do
            let !stateSpec = bStateSpecs blueprint V.! stateName
            val <- readTape tape0 cursor
            let !branch = if val then ssIfOne stateSpec else ssIfZero stateSpec
            tape1 <- writeTape tape0 cursor (ssWrite branch)
            let !cursor1 = if ssLeft branch then cursor - 1 else cursor + 1
            go tape1 (steps + 1) cursor1 (ssNext branch)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    blueprint <- readBlueprint IO.stdin
    putStrLn $ "Checksum: " ++ show (runBlueprint blueprint)
