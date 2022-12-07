{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

import           AdventOfCode.Main
import           Data.Foldable     (toList)
import           Data.List         (foldl', sort)
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Sum (..))
import           Text.Read         (readMaybe)

--------------------------------------------------------------------------------

data FsTree k v = FsTree v (M.Map k (FsTree k v))
    deriving (Eq, Foldable, Functor, Show)

type FsZipper k v = ([(FsTree k v, k)], FsTree k v)

singleton :: v -> FsTree k v
singleton v = FsTree v M.empty

value :: FsTree k v -> v
value (FsTree v _) = v

toZipper :: FsTree k v -> FsZipper k v
toZipper t = ([], t)

toTree :: Ord k => FsZipper k v -> FsTree k v
toTree = snd . top

up :: Ord k => FsZipper k v -> Maybe (FsZipper k v)
up ([], _)                           = Nothing
up ((FsTree pval pkids, k) : ps, c) =
    Just (ps, FsTree pval (M.insert k c pkids))

top :: Ord k => FsZipper k v -> FsZipper k v
top z = maybe z top $ up z

down :: Ord k => k -> FsZipper k v -> Maybe (FsZipper k v)
down k (ps, FsTree v cs) = case M.lookup k cs of
    Nothing -> Nothing
    Just c  -> Just ((FsTree v (M.delete k cs), k) : ps, c)

insert :: Ord k => k -> FsTree k v -> FsZipper k v -> Maybe (FsZipper k v)
insert k c (ps, FsTree v cs) = case M.lookup k cs of
    Nothing -> Just (ps, FsTree v (M.insert k c cs))
    Just _  -> Nothing

--------------------------------------------------------------------------------

data Node = Dir | File Int deriving (Eq, Show)

replay :: String -> FsZipper String Node
replay = foldl' go (toZipper (singleton Dir)) . lines
  where
    go z line = case words line of
        ["$", "cd", "/"] -> top z
        ["$", "cd", ".."] -> fromMaybe (error ".. from root") $ up z
        ["$", "cd", dir] -> fromMaybe (error $ "cd into " ++ dir) $ down dir z
        ["$", "ls"] -> z
        ["dir", dir] -> fromMaybe z $ insert dir (singleton Dir) z
        [size, file] | Just s <- readMaybe size -> fromMaybe z $
            insert file (singleton $ File s) z
        _ -> error $ "unknown command: " ++ line

bottomUp :: (Ord k, Monoid m) => (v -> m) -> FsTree k v -> FsTree k (v, m)
bottomUp f (FsTree v cs) =
    let cs'   = M.map (bottomUp f) cs
        total = f v <> mconcat [x | (_, FsTree (_, x) _) <- M.toList cs'] in
    FsTree (v, total) cs'

main :: IO ()
main = simpleMain $ \input ->
    let tree0 = toTree $ replay input
        tree1 = bottomUp (\n -> case n of File s -> Sum s; Dir -> 0) tree0
        dirs  = map snd . filter ((== Dir) . fst) $ toList tree1
        part1 = getSum . mconcat $ filter (<= 100000) dirs
        part2 = head . sort $ filter (>= delete) $ map getSum dirs

        total    = 70000000
        required = 30000000
        used     = getSum . snd $ value tree1
        unused   = total - used
        delete   = required - unused in
    (part1, part2)
