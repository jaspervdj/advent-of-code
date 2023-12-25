import           AdventOfCode.Dijkstra   (Bfs (..), bfs)
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
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

parseGraph :: NP.Parser Char (Graph String)
parseGraph = fmap mkGraph $ flip NP.sepBy1 NP.newline $ (,)
    <$> (node <* NP.char ':')
    <*> NP.many1 (NP.char ' ' *> node)
  where
    node      = NP.many1 NP.alpha
    mkGraph l = foldl'
        (\acc e -> insert e acc)
        M.empty
        [mkEdge x y | (x, ys) <- l, y <- ys]

-- | Starts a BFS from every single node in the graph, and count how many times
-- we took each edge accross all BFSs.
popularity :: Ord a => Graph a -> M.Map (Edge a) Int
popularity graph = M.fromListWith (+) $ do
    (start, _) <- M.toList graph
    (_, distances) <- M.toList $ bfsDistances $ bfs
        (\x -> maybe [] S.toList $ M.lookup x graph)
        (const False)
        start
    edge <- zipWith mkEdge distances (drop 1 distances)
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
        cutSize = M.size $ bfsDistances $ bfs
            (\x -> maybe [] S.toList $ M.lookup x cut)
            (const False)
            start
    pure (pure (cutSize * (graphSize - cutSize)), pure "fin")
