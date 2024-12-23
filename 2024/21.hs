{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (pureMain)
import           AdventOfCode.V2     (V2 (..))
import           Control.Monad.State (State, gets, modify, runState, state)
import           Data.Kind           (Type)
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Data.Maybe          (maybeToList)
import qualified Data.Set            as S
import           Data.Traversable    (for)
import           Text.Read           (readMaybe)

-- | Use a dedicated datatype for digits so we can write total rather than
-- partial functions and have a convenient @Bounded@ instance.
data N = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
    deriving (Bounded, Enum, Read, Show)

-- | Either the enter key (A) or another key (where @i@ stands for input).
data K i = A | K i deriving (Eq, Ord, Show)

-- | A sequence of digit keys ending with enter (A).
newtype Sequence = Sequence [N] deriving (Show)

instance Read Sequence where
    readsPrec _ str = maybeToList $ case break (== 'A') str of
        (digits, 'A' : remainder) -> do
            ns <- traverse (\d -> readMaybe ['N', d]) digits
            pure (Sequence ns, remainder)
        _ -> Nothing

numeric :: Sequence -> Int
numeric (Sequence ns) = foldl' (\acc n -> acc * 10 + fromEnum n) 0 ns

-- | A list of alternatives keypresses that will accomplish the same thing.
-- Having this gives some more semantics to the nested lists to improve
-- readability.
newtype Alternatives a = Alternatives [a]
    deriving (Functor, Foldable, Traversable)

-- | A keypad finds buttons to touch to navigate in between two keys.
-- We can think of @k@ as the "input" to the keyboard (the buttons we want
-- to press) and @d@ as the output (possible that buttons could be pressed
-- to achieve this).
--
-- Capturing both of these in the type allows us to build type-safe stacks of
-- keyboards, where each one feeds into the next.
type Keypad i o = [i] -> [Alternatives [o]]

-- | Generate a keypad that outputs directions, based on a total function that
-- generates positions for the keys.
mkKeypad :: forall i. (Bounded i, Enum i) => (K i -> G.Pos) -> Keypad i G.Dir
mkKeypad pos =
    -- Start navigating from the enter key and end back there.
    \s -> zipWith navigate (A : map K s) (map K s ++ [A])
  where
    keys = A : map K [minBound .. maxBound]
    grid = S.fromList $ map pos keys

    navigate :: K i -> K i -> Alternatives [G.Dir]
    navigate src dst
        | x0 == x1  = Alternatives [vert]
        | y0 == y1  = Alternatives [hori]
        | otherwise = Alternatives $
            [(hori ++ vert) | V2 x1 y0 `S.member` grid] ++
            [(vert ++ hori) | V2 x0 y1 `S.member` grid]
      where
        V2 x0 y0 = pos src
        V2 x1 y1 = pos dst
        hori     = replicate (abs (x0 - x1)) $ if x0 < x1 then G.R else G.L
        vert     = replicate (abs (y0 - y1)) $ if y0 < y1 then G.D else G.U

numKeypad :: Keypad N G.Dir
numKeypad = mkKeypad $ \key -> case key of
    K N7 -> V2 0 0; K N8 -> V2 1 0; K N9 -> V2 2 0
    K N4 -> V2 0 1; K N5 -> V2 1 1; K N6 -> V2 2 1
    K N1 -> V2 0 2; K N2 -> V2 1 2; K N3 -> V2 2 2
    K                 N0 -> V2 1 3; A    -> V2 2 3

dirKeypad :: Keypad G.Dir G.Dir
dirKeypad = mkKeypad $ \key -> case key of
    K                  G.U -> V2 1 0; A     -> V2 2 0
    K G.L -> V2 0 1; K G.D -> V2 1 1; K G.R -> V2 2 1

-- | A type-safe stack of keyboards.  We use a list to indicate what sort of
-- buttons are used in each layer.
data Stack (l :: [Type]) where
    -- | A single-element stack.  This only contains a keypad.
    SSingle :: Keypad i o -> Stack '[i, o]

    -- | A keyboard on top of a stack.  This contains a keypad, a cache, and
    -- the next stack.
    SCons
        :: Ord o
        => Keypad i o -> M.Map [o] Int -> Stack (o ': t)
        -> Stack (i ': o ': t)

scons :: Ord o => Keypad i o -> Stack (o ': t) -> Stack (i ': o ': t)
scons kp = SCons kp mempty

shortest :: forall i t. [i] -> Stack (i ': t) -> (Int, Stack (i ': t))
shortest keys stack = case stack of
    SSingle keypad ->
        let len = sum $ map (succ . minimum . fmap length) $ keypad keys in
        (len, stack)
    SCons keypad cache next ->
        let (len, (cache', next')) = runState
                (fmap sum $ mapM descend $ keypad keys)
                (cache, next) in
        (len, SCons keypad cache' next')
  where
    descend
        :: forall o t'. Ord o
        => Alternatives [o] -> State (M.Map [o] Int, Stack (o ': t')) Int
    descend alts = fmap minimum $ for alts $ \dirs -> cached dirs $ do
        next <- gets snd
        let (len, next') = shortest dirs next
        modify $ \(cache, _) -> (cache, next')
        pure len

    cached
        :: forall o t'. Ord o
        => [o]
        -> State (M.Map [o] Int, Stack (o ': t')) Int
        -> State (M.Map [o] Int, Stack (o ': t')) Int
    cached k f = do
        mbLen <- gets (M.lookup k . fst)
        case mbLen of
            Just len -> pure len
            Nothing -> do
                len <- f
                modify $ \(cache, next) -> (M.insert k len cache, next)
                pure len

main :: IO ()
main = pureMain $ \str -> do
    sequences <- maybe (Left "no parse") pure $ traverse readMaybe $ lines str

    let stack1 :: Stack '[N, G.Dir, G.Dir, G.Dir]
        stack1 = scons numKeypad $ scons dirKeypad $ SSingle dirKeypad
        stack2 = scons numKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            scons dirKeypad $ scons dirKeypad $ scons dirKeypad $
            SSingle dirKeypad

        solve :: forall t. Stack (N ': t) -> Int
        solve stack = fst $ flip runState stack $ fmap sum $
            for sequences $ \s@(Sequence ns) -> do
                len <- state (shortest ns)
                pure $ numeric s * len

    pure (pure (solve stack1), pure (solve stack2))
