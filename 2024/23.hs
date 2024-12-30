import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import           Control.Monad           (guard)
import           Data.Foldable           (maximumBy, toList)
import           Data.List               (intercalate, sort)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Ord                (comparing)
import qualified Data.Set                as S

data Node = Node {-# UNPACK #-} !Char {-# UNPACK #-} !Char
    deriving (Eq, Ord)

instance Show Node where
    show (Node x y) = [x, y]

parseEdges :: NP.Parser Char [(Node, Node)]
parseEdges =
    many $ (,) <$> parseNode <* NP.char '-' <*> parseNode <* NP.spaces
  where
    parseNode = Node <$> NP.alpha <*> NP.alpha

newtype Graph v = Graph (M.Map v (S.Set v))

instance Ord v => Semigroup (Graph v) where
    Graph l <> Graph r = Graph $ M.unionWith S.union l r

instance Ord v => Monoid (Graph v) where
    mempty = Graph M.empty

graphNodes :: Graph v -> [v]
graphNodes (Graph m) = M.keys m

graphFromEdge :: Ord v => v -> v -> Graph v
graphFromEdge x y = Graph $ M.fromList [(x, S.singleton y), (y, S.singleton x)]

data Three v = Three v v v deriving (Eq, Ord)

threes :: Ord v => v -> Graph v -> [Three v]
threes x (Graph g) = do
    y <- maybe [] toList $ M.lookup x g
    z <- filter (/= x) $ maybe [] toList $ M.lookup y g
    guard $ maybe False (x `S.member`) $ M.lookup z g
    [a, b, c] <- pure $ sort [x, y, z]
    pure $ Three a b c

prettyNodeSet :: S.Set Node -> String
prettyNodeSet = intercalate "," . map show . S.toAscList

bronKerbosch :: forall v. Ord v => Graph v -> [S.Set v]
bronKerbosch (Graph m) = bronKerbosch1 S.empty (M.keysSet m) S.empty
  where
    bronKerbosch1 :: S.Set v -> S.Set v -> S.Set v -> [S.Set v]
    bronKerbosch1 r p x = [r | S.null p && S.null x] ++ go r p x (S.toList p)

    go :: S.Set v -> S.Set v -> S.Set v -> [v] -> [S.Set v]
    go _ _ _ []       = []
    go r p x (v : vs) =
        bronKerbosch1
            (S.insert v r)
            (S.intersection p (neighbours v))
            (S.intersection x (neighbours v)) ++
        go r (S.delete v p) (S.insert v x) vs

    neighbours :: v -> S.Set v
    neighbours v = fromMaybe mempty $ M.lookup v m

main :: IO ()
main = pureMain $ \str -> do
    edges <- NP.runParser parseEdges str
    let graph = foldMap (uncurry graphFromEdge) edges
        nodes = graphNodes graph

        ts    = [n | n@(Node 't' _) <- nodes]
        part1 = S.fromList $ concatMap (`threes` graph) ts
        part2 = maximumBy (comparing S.size) $ bronKerbosch graph

    pure (pure (S.size part1), pure (prettyNodeSet part2))
