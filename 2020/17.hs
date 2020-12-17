import qualified AdventOfCode.Grid as Grid
import           AdventOfCode.Main (simpleMain)
import           AdventOfCode.V2   (V2 (..))
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

data Vec3 = Vec3
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    deriving (Eq, Ord)

traverseVec3 :: Applicative f => (Int -> f Int) -> Vec3 -> f Vec3
traverseVec3 f (Vec3 x y z) = Vec3 <$> f x <*> f y <*> f z

data Vec4 = Vec4
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    deriving (Eq, Ord)

traverseVec4 :: Applicative f => (Int -> f Int) -> Vec4 -> f Vec4
traverseVec4 f (Vec4 x y z w) = Vec4 <$> f x <*> f y <*> f z <*> f w

class TraversableVec t where
    traverseVec :: Applicative f => (Int -> f Int) -> t -> f t

instance TraversableVec Vec3 where traverseVec = traverseVec3
instance TraversableVec Vec4 where traverseVec = traverseVec4

neighbours :: (TraversableVec t, Eq t) => t -> [t]
neighbours x = filter (/= x) $ traverseVec (\i -> [i, i - 1, i + 1]) x

step :: (TraversableVec t, Ord t) => Set t -> Set t
step active0 =
    Set.filter isActive . Set.fromList . concatMap neighbours $
    Set.toList active0
  where
    isActive x
        | x `Set.member` active0 = nbs >= 2 && nbs <= 3
        | otherwise              = nbs == 3
      where
        nbs = length . filter (`Set.member` active0) $ neighbours x

main :: IO ()
main = simpleMain $ \inputstr ->
    let grid2d = Map.keysSet . Map.filter (== '#') $ Grid.fromString inputstr
        toVec3 (V2 x y) = Vec3 x y 0
        toVec4 (V2 x y) = Vec4 x y 0 0 in
    ( Set.size $ iterate step (Set.map toVec3 grid2d) !! 6
    , Set.size $ iterate step (Set.map toVec4 grid2d) !! 6
    )
