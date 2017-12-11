{-# LANGUAGE BangPatterns #-}
import           Control.Arrow ((&&&))
import           Data.Function (on)
import           Data.List     (foldl')
import           Data.Maybe    (maybeToList)
import qualified System.IO     as IO
import           Text.Read     (readMaybe)

--------------------------------------------------------------------------------

data Component = Component !Int !Int deriving (Show)

readComponents :: IO.Handle -> IO [Component]
readComponents h =
    IO.hGetContents h >>= mapM parseComponent . lines
  where
    parseComponent line =
        maybe (fail $ "Could not parse component: " ++ show line) return $
        case break (== '/') line of
            (x, '/' : y) -> Component <$> readMaybe x <*> readMaybe y
            _            -> Nothing

--------------------------------------------------------------------------------

data Bridge = Bridge
    { bridgeStrength :: !Int
    , bridgeLength   :: !Int
    , bridgePort     :: !Int
    } deriving (Show)

zero :: Bridge
zero = Bridge {bridgeStrength = 0, bridgeLength = 0, bridgePort = 0}

connect :: Component -> Bridge -> Maybe Bridge
connect (Component x y) (Bridge total len z)
    | x == z    = Just (Bridge (total + x + y) (len + 1) y)
    | y == z    = Just (Bridge (total + x + y) (len + 1) x)
    | otherwise = Nothing

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

bridges :: [Component] -> [Bridge]
bridges = go zero
  where
    go :: Bridge -> [Component] -> [Bridge]
    go bridge0 components0 =
        bridge0 :
        [ extended
        | (c, components1) <- select components0
        , bridge1          <- maybeToList $ connect c bridge0
        , extended         <- go bridge1 components1
        ]

--------------------------------------------------------------------------------

newtype Strongest = Strongest {unStrongest :: Bridge}

instance Eq Strongest where
    (==) = (==) `on` (bridgeStrength . unStrongest)

instance Ord Strongest where
    compare = compare `on` (bridgeStrength . unStrongest)

newtype Longest = Longest {unLongest :: Bridge}

instance Eq Longest where
    (==) = (==) `on` ((bridgeLength &&& bridgeStrength) . unLongest)

instance Ord Longest where
    compare = compare `on` ((bridgeLength &&& bridgeStrength) . unLongest)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    components <- readComponents IO.stdin

    -- Attempt to do everything in one fold so we don't need to keep all bridge
    -- combinations in memory.
    let (Strongest strongest, Longest longest) = foldl'
            (\(!s0, !l0) !bridge ->
                (max (Strongest bridge) s0, max (Longest bridge) l0))
            (Strongest zero, Longest zero)
            (bridges components)

    putStrLn $ "Strongest bridge: " ++ show (bridgeStrength strongest)
    putStrLn $ "Longest bridge: " ++ show (bridgeStrength longest)
