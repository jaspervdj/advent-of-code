import qualified AdventOfCode.Dijkstra   as Dijkstra
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Data.Foldable           (toList)
import           Data.List               (foldl', sortOn)
import qualified Data.Map                as M
import           Data.Ord                (Down (..))
import qualified Data.Set                as S

data Edge a = Edge a a deriving (Eq, Ord, Show)

mkEdge :: Ord a => a -> a -> Edge a
mkEdge x y = if x <= y then Edge x y else Edge y x

type Graph a = M.Map a (S.Set a)

insert :: Ord a => Edge a -> Graph a -> Graph a
insert (Edge x y) =
    M.insertWith S.union x (S.singleton y) .
    M.insertWith S.union y (S.singleton x)

delete :: Ord a => Edge a -> Graph a -> Graph a
delete (Edge x y) = M.adjust (S.delete y) x . M.adjust (S.delete x) y

bfs :: Ord a => a -> Graph a -> [[a]]
bfs start graph = map snd . M.toList . Dijkstra.bfsDistances $ Dijkstra.bfs
    (\x -> maybe [] S.toList $ M.lookup x graph) (const False) start

parseGraph :: NP.Parser Char (Graph String)
parseGraph = fmap mkGraph $ flip NP.sepBy1 NP.newline $ (,)
    <$> (node <* NP.char ':')
    <*> (toList <$> NP.many1 (NP.char ' ' *> node))
  where
    node      = toList <$> NP.many1 NP.alpha
    mkGraph l = foldl'
        (\acc e -> insert e acc)
        M.empty
        [mkEdge x y | (x, ys) <- l, y <- ys]

-- | Starts a BFS from every single node in the graph, and count how many times
-- we took each edge accross all BFSs.
popularity :: Ord a => Graph a -> M.Map (Edge a) Int
popularity graph = M.fromListWith (+) $ do
    (start, _) <- M.toList graph
    paths <- bfs start graph
    edge <- zipWith mkEdge paths (drop 1 paths)
    pure (edge, 1)

main :: IO ()
main = pureMain $ \str -> do
    graph <- NP.runParser parseGraph str
    start <- case M.toList graph of
        []         -> Left "empty graph"
        (x, _) : _ -> Right x
    let pop = take 3 $ sortOn (Down . snd) $ M.toList $ popularity graph
        cut = foldl' (\acc e -> delete e acc) graph $ map fst pop
        graphSize = M.size graph
        cutSize = length $ bfs start cut
    pure (pure (cutSize * (graphSize - cutSize)), pure "fin")
