import qualified Data.Graph as Graph
import           Data.Maybe (mapMaybe)
import           Text.Read  (readMaybe)

data Spec a = Spec a [a] deriving (Show)

parseSpec :: Read a => String -> Maybe (Spec a)
parseSpec txt = case words (filter (/= ',') txt) of
    x : "<->" : ys -> Spec <$> readMaybe x <*> traverse readMaybe ys
    _              -> Nothing

specsToGraph :: Ord a => [Spec a] -> (Graph.Graph, a -> Maybe Graph.Vertex)
specsToGraph specs =
    let (g, _, f) = Graph.graphFromEdges [(x, x, ys) | Spec x ys <- specs] in
    (g, f)

main :: IO ()
main = do
    specs <- mapMaybe parseSpec . lines <$> getContents
    let (graph, findVertex) = specsToGraph specs
    case findVertex (0 :: Int) of
        Nothing -> fail "0 not present"
        Just v0 -> do
            let reachable = Graph.reachable graph v0
            putStrLn $ "Number of programs: " ++ show (length reachable)
    let components = Graph.components graph
    putStrLn $ "Number of components: " ++ show (length components)
