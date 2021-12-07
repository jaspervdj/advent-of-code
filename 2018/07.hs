import           Data.Char       (ord, toLower)
import qualified Data.List       as L
import           Data.List.Extra (select)
import qualified Data.Map        as M
import           Data.Maybe      (listToMaybe)
import qualified System.IO       as IO

data Dependencies a = Dependencies [a] [(a, a)] deriving (Show)

mkDependencies :: Ord a => [(a, a)] -> Dependencies a
mkDependencies list = Dependencies
    (L.sort $ L.nub [i | (x, y) <- list, i <- [x, y]]) list

pop :: Eq a => Dependencies a -> Maybe (a, Dependencies a)
pop deps = do
    (x, deps') <- pick deps
    return (x, done x deps')

-- | Pick and item from the dependencies.
pick :: Eq a => Dependencies a -> Maybe (a, Dependencies a)
pick (Dependencies items deps) = listToMaybe
    [ (i, Dependencies items' deps)
    | (i, items') <- select items
    , not $ or [y == i | (_, y) <- deps]
    ]

-- | Mark an item as done.
done :: Eq a => a -> Dependencies a -> Dependencies a
done i (Dependencies items deps) =
    Dependencies items [(x, y) | (x, y) <- deps, x /= i]

parseDependencies :: IO.Handle -> IO (Dependencies Char)
parseDependencies h =
    IO.hGetContents h >>= fmap mkDependencies . mapM parseDependency . lines
  where
    parseDependency input = case words input of
        [ "Step", [x], "must", "be", "finished", "before", "step",
                [y], "can", "begin."] -> return (x, y)
        _ -> fail $ "Could not parse line: " ++ show input

type Time = Int

run :: Eq a
    => (a -> Time)                    -- ^ Time for an item
    -> Int                            -- ^ Max workers
    -> Time                           -- ^ Current time
    -> (Dependencies a, M.Map Int a)
    -> Time
run ft num time (deps, workers)
    | M.size workers < num, Just (i, deps') <- pick deps =
        run ft num time (deps', M.insert (time + ft i) i workers)

    | Just ((t, i), workers') <- M.minViewWithKey workers =
        run ft num t (done i deps, workers')

    | M.null workers, Nothing <- pop deps = time

    | otherwise = error "Cycle or some other weird stuff"

main :: IO ()
main = do
    deps <- parseDependencies IO.stdin
    putStrLn $ L.unfoldr pop deps
    print $ run duration 5 0 (deps, M.empty)
  where
    duration c = 60 + ord (toLower c) - ord 'a' + 1
