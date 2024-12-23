import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.List               (intercalate, sort)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as S
import           System.Random           (RandomGen, mkStdGen, randomR)

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

randomPop :: RandomGen g => S.Set a -> g -> (a, S.Set a, g)
randomPop set gen0 =
    let (idx, gen1) = randomR (0, S.size set - 1) gen0 in
    (S.elemAt idx set, S.deleteAt idx set, gen1)

randomComplete
    :: forall v g. (Ord v, RandomGen g) => v -> Graph v -> g -> (S.Set v, g)
randomComplete start (Graph m) =
    go (S.singleton start) S.empty (neighbours start)
  where
    go :: S.Set v -> S.Set v -> S.Set v -> g -> (S.Set v, g)
    go complete outside fringe0 gen0
        | S.null fringe0 = (complete, gen0)
        | not valid      = go complete (S.insert next outside) fringe1 gen1
        | otherwise      = go (S.insert next complete) outside fringe2 gen1
      where
         (next, fringe1, gen1) = randomPop fringe0 gen0
         fringe2 = fringe1 <> (neighbours next `S.difference` outside)
         valid = S.null $ complete `S.difference` neighbours next

    neighbours :: v -> S.Set v
    neighbours v = fromMaybe mempty $ M.lookup v m

randomSearch
    :: forall v g. (Show v, Ord v, RandomGen g)
    => Int -> Graph v -> g -> (S.Set v, g)
randomSearch maxIterations graph@(Graph g) = go 0 S.empty
  where
    nodes = M.keysSet g
    go i best gen0 | i >= maxIterations = (best, gen0)
    go i best gen0 =
        let (start, _, gen1) = randomPop nodes gen0
            (comp, gen2)     = randomComplete start graph gen1 in
        go (i + 1) (if S.size comp > S.size best then comp else best) gen2

prettyNodeSet :: S.Set Node -> String
prettyNodeSet = intercalate "," . map show . S.toAscList

main :: IO ()
main = pureMain $ \str -> do
    edges <- NP.runParser parseEdges str
    let graph = foldMap (uncurry graphFromEdge) edges
        nodes = graphNodes graph

        ts    = [n | n@(Node 't' _) <- nodes]
        part1 = S.fromList $ concatMap (`threes` graph) ts

        seed          = mkStdGen 1
        maxIterations = length nodes * length nodes
        (part2, _)    = randomSearch maxIterations graph seed
    pure (pure (S.size part1), pure (prettyNodeSet part2))
