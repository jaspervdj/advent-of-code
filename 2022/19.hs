{-# LANGUAGE TypeFamilies #-}
import           AdventOfCode.BranchAndBound
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser     as NP
import           Control.Applicative         ((<|>))
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, mapMaybe)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU

newtype Resource = Resource Int deriving (Eq, Ord, Show)

ore, clay, obsidian, geode :: Resource
ore      = Resource 0
clay     = Resource 1
obsidian = Resource 2
geode    = Resource 3

allResources :: [Resource]
allResources = [ore, clay, obsidian, geode]

newtype Resources = Resources (VU.Vector Int) deriving (Show)

resourcesFromList :: [(Resource, Int)] -> Resources
resourcesFromList list = Resources $ VU.fromList $ do
    r <- [ore, clay, obsidian, geode]
    pure $ fromMaybe 0 (M.lookup r freq)
  where
    freq = M.fromListWith (+) list

(|!) :: Resources -> Resource -> Int
Resources rs |! Resource i = rs VU.! i

(|+) :: Resources -> Resources -> Resources
Resources l |+ Resources r = Resources (VU.zipWith (+) l r)

(|-) :: Resources -> Resources -> Maybe Resources
Resources l |- Resources r =
    let x = VU.zipWith (-) l r in
    if VU.all (>= 0) x then Just (Resources x) else Nothing

(|++) :: Resources -> Resource -> Resources
Resources rs |++ Resource i = Resources (rs VU.// [(i, (rs VU.! i) + 1)])

newtype ByResource a = ByResource (V.Vector a) deriving (Show)

byResourceFromList :: [(Resource, a)] -> ByResource a
byResourceFromList list = ByResource $ V.fromList $ do
    r <- [ore, clay, obsidian, geode]
    pure $ fromMaybe
        (error $ "byResourceFromList: no entry for " ++ show r)
        (lookup r list)

byResourceIndex :: ByResource a -> Resource -> a
byResourceIndex (ByResource br) (Resource i) = br V.! i

data Blueprint = Blueprint Int (ByResource Resources) deriving (Show)

parseBlueprints :: NP.Parser Char [Blueprint]
parseBlueprints = NP.many1 blueprint
  where
    blueprint = Blueprint
        <$> (NP.string "Blueprint " *> NP.decimal <* NP.char ':' <* NP.spaces)
        <*> (byResourceFromList <$> NP.many1 (robot <* NP.spaces))
    robot = (,)
        <$> (NP.string "Each " *> resource <* NP.string " robot costs ")
        <*> (cost <* NP.char '.')
    cost = fmap resourcesFromList $ NP.sepBy1
        ((\i r -> (r, i)) <$> (NP.decimal <* NP.char ' ') <*> resource)
        (NP.string " and ")
    resource =
        (ore      <$ NP.string "ore")      <|>
        (clay     <$ NP.string "clay")     <|>
        (obsidian <$ NP.string "obsidian") <|>
        (geode    <$ NP.string "geode")

data Inventory = Inventory
    { invRobots    :: !Resources
    , invResources :: !Resources
    } deriving (Show)

data Problem = Problem !Blueprint !Inventory !Int deriving (Show)

wait :: Problem -> Problem
wait (Problem blueprint inv tl) = Problem
    blueprint inv {invResources = invResources inv |+ invRobots inv} (tl - 1)

build :: Problem -> Resource -> Maybe Problem
build (Problem blueprint@(Blueprint _ costs) inv timeLeft) robot
    | timeLeft <= 0 = Nothing
    | Just remainder <- invResources inv |- cost =
        let accum = remainder |+ invRobots inv in
        Just $ Problem
             blueprint
             inv {invResources = accum, invRobots = invRobots inv |++ robot}
             (timeLeft - 1)
    | any missing allResources = Nothing
    | otherwise = do
        let accum = invResources inv |+ invRobots inv
        build (Problem blueprint inv {invResources = accum} (timeLeft - 1)) robot
  where
    cost = byResourceIndex costs robot
    missing r = cost |! r > 0 && invRobots inv |! r <= 0

instance BranchAndBound Problem where
    type Score Problem = Int

    score (Problem _ inv _) = invResources inv |! geode

    potential problem@(Problem _ inv timeLeft) =
        score problem +
        (invRobots inv |! geode) * timeLeft +
        (((timeLeft) * (timeLeft - 1)) `div` 2)

    next problem@(Problem _ _ timeLeft)
        | timeLeft <= 0 = []
        | null builds   = [wait problem]
        | otherwise     = builds
      where
        builds = mapMaybe (build problem) [geode, obsidian, clay, ore]

main :: IO ()
main = pureMain $ \input -> do
    blueprints <- NP.runParser parseBlueprints input
    let inv0  = Inventory (resourcesFromList [(ore, 1)]) (resourcesFromList [])

        part1 = sum $ do
            blueprint@(Blueprint bid _) <- blueprints
            let geodes = score . branchAndBound $ Problem blueprint inv0 24
            pure $ bid * geodes

        part2 = product $ do
            blueprint <- take 3 blueprints
            pure . score . branchAndBound $ Problem blueprint inv0 32

    pure (pure part1, pure part2)
