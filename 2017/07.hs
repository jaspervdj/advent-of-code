{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
import           Control.Monad (guard, forM)
import qualified Data.Graph    as G
import qualified Data.Map      as M
import           Data.Maybe    (listToMaybe, mapMaybe)
import qualified Data.Tree     as Tree

type Label = String

newtype Weight = Weight Int deriving (Eq, Num, Ord, Show)

data Spec = Spec Label Weight [Label]

data Graph = Graph
    { gGraph        :: G.Graph
    , gLookupLabel  :: G.Vertex -> Label
    , gLookupWeight :: G.Vertex -> Weight
    }

parseSpec :: String -> Maybe Spec
parseSpec txt = case words (filter (/= ',') txt) of
    x : w : []        -> Just (Spec x (Weight $ read $ init $ tail w) [])
    x : w : "->" : ys -> Just (Spec x (Weight $ read $ init $ tail w) ys)
    _                 -> Nothing

specsToGraph :: [Spec] -> Graph
specsToGraph specs =
    let (g, f, _) = G.graphFromEdges [(w, x, ys) | Spec x w ys <- specs] in
    Graph
        { gGraph        = g
        , gLookupLabel  = \v -> let (_, l, _) = f v in l
        , gLookupWeight = \v -> let (w, _, _) = f v in w
        }

balance :: Graph -> Tree.Tree G.Vertex -> Either Weight Weight
balance graph (Tree.Node vertex children0) = do
    -- Balance children recursively.
    childWeights <- forM children0 $ \child -> do
        cumulative <- balance graph child
        return (cumulative, Tree.rootLabel child)

    -- Check if we need to make a correction
    let weights = map fst childWeights
    case correction weights of
        -- One element is weird.  Find the child and patch its weight.
        Just (weird, diff) | Just child <- lookup weird childWeights ->
            Left $ gLookupWeight graph child + diff

        -- All is fine...
        _ -> return $ selfWeight + sum weights

  where
    selfWeight = gLookupWeight graph vertex

    -- Return the one weird element in the list, and the diff to the normal
    -- elements.
    correction :: [Weight] -> Maybe (Weight, Weight)
    correction xs = do
        let freqs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]
        guard $ length freqs >= 2
        common <- listToMaybe [x | (x, f) <- freqs, f > (1 :: Int)]
        weird  <- listToMaybe [x | (x, f) <- freqs, f == 1]
        return (weird, common - weird)

main :: IO ()
main = do
    specs <- mapMaybe parseSpec . lines <$> getContents
    let graph           = specsToGraph specs
        (rootv : _)     = G.topSort (gGraph graph)
        (tree : _)      = G.dfs (gGraph graph) [rootv]
        Left (Weight w) = balance graph tree

    putStrLn $ "Root: " ++ gLookupLabel graph rootv
    putStrLn $ "Weight: " ++ show w
