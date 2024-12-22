import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (pureMain)
import           AdventOfCode.V2     (V2 (..))
import           Control.Monad.State
import           Data.List           (foldl')
import qualified Data.Map            as M
import           Data.Maybe          (maybeToList)
import qualified Data.Set            as S
import           Data.Traversable    (for)
import           Text.Read           (readMaybe)

data N = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
    deriving (Bounded, Enum, Read, Show)

data K k = A | K k deriving (Eq, Ord, Show)

newtype Sequence = Sequence [N] deriving (Show)

instance Read Sequence where
    readsPrec _ str = maybeToList $ case break (== 'A') str of
        (digits, 'A' : remainder) -> do
            ns <- traverse (\d -> readMaybe ['N', d]) digits
            pure (Sequence ns, remainder)
        (_, _) -> Nothing

numeric :: Sequence -> Int
numeric (Sequence ns) = foldl' (\acc n -> acc * 10 + fromEnum n) 0 ns

data Keypad k = Keypad
    { kpGrid :: S.Set G.Pos
    , kpPos  :: K k -> G.Pos
    }

mkKeypad :: (Bounded k, Enum k) => (K k -> G.Pos) -> Keypad k
mkKeypad f = Keypad (S.fromList $ map f keys) f
  where
    keys = A : map K [minBound .. maxBound]

numKeypad :: Keypad N
numKeypad = mkKeypad $ \key -> case key of
    K N7 -> V2 0 0; K N8 -> V2 1 0; K N9 -> V2 2 0
    K N4 -> V2 0 1; K N5 -> V2 1 1; K N6 -> V2 2 1
    K N1 -> V2 0 2; K N2 -> V2 1 2; K N3 -> V2 2 2
    K                 N0 -> V2 1 3; A    -> V2 2 3

dirKeypad :: Keypad G.Dir
dirKeypad = mkKeypad $ \key -> case key of
    K                  G.U -> V2 1 0; A     -> V2 2 0
    K G.L -> V2 0 1; K G.D -> V2 1 1; K G.R -> V2 2 1

newtype Dirs = Dirs [G.Dir] deriving (Eq, Ord)

navigate :: Keypad k -> K k -> K k -> [Dirs]
navigate kp src dst
    | x0 == x1  = [Dirs vert]
    | y0 == y1  = [Dirs hori]
    | otherwise =
        [Dirs (hori ++ vert) | V2 x1 y0 `S.member` kpGrid kp] ++
        [Dirs (vert ++ hori) | V2 x0 y1 `S.member` kpGrid kp]
  where
    V2 x0 y0 = kpPos kp src
    V2 x1 y1 = kpPos kp dst
    hori     = replicate (abs (x0 - x1)) $ if x0 < x1 then G.R else G.L
    vert     = replicate (abs (y0 - y1)) $ if y0 < y1 then G.D else G.U

data Stack k = Stack
    { sKeypad :: Keypad k
    , sCache  :: M.Map Dirs Int
    , sNext   :: Maybe (Stack G.Dir)
    }

singleton :: Keypad k -> Stack k
singleton l = Stack l mempty Nothing

addKeypad :: Keypad k -> Stack G.Dir -> Stack k
addKeypad l stack = Stack l mempty $ Just stack

shortest :: [k] -> Stack k -> (Int, Stack k)
shortest keys = runState $ fmap sum $ mapM work $
    zip (A : map K keys) (map K keys ++ [A])
  where
    cached k f = do
        mbLen <- gets (M.lookup k . sCache)
        case mbLen of
            Just len -> pure len
            Nothing -> do
                len <- f
                modify $ \r -> r {sCache = M.insert k len (sCache r)}
                pure len

    work (from, to) = do
        keypad <- gets sKeypad
        let options = navigate keypad from to
        mbNext <- gets sNext
        fmap minimum $ case mbNext of
            Nothing -> pure [length ds + 1 | Dirs ds <- options]
            Just next -> for options $ \dirs@(Dirs ds) -> cached dirs $ do
                let (len, next') = shortest ds next
                modify $ \r -> r {sNext  = Just next'}
                pure len

main :: IO ()
main = pureMain $ \str -> do
    sequences <- maybe (Left "no parse") pure $ traverse readMaybe $ lines str

    let dirs n = iterate (addKeypad dirKeypad) (singleton dirKeypad) !! (n - 1)
        stack2  = addKeypad numKeypad $ dirs (2 :: Int)
        stack25 = addKeypad numKeypad $ dirs (25 :: Int)

        solve stack = flip evalState stack $ fmap sum $
            for sequences $ \s@(Sequence ns) -> do
                len <- state (shortest ns)
                pure $ numeric s * len

    pure (pure (solve stack2), pure (solve stack25))
