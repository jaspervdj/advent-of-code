{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
import qualified AdventOfCode.Grid   as Grid
import           AdventOfCode.Main   (simpleMain)
import           AdventOfCode.V2     (V2 (..))
import           Control.Monad       (guard)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Vector.Unboxed as VU
import           GHC.TypeLits        (Nat)

type Cubes (n :: Nat) = Set (Vec n)
newtype Vec (n :: Nat) = Vec (VU.Vector Int) deriving (Eq, Ord)

vec3 :: V2 Int -> Vec 3
vec3 (V2 x y) = Vec $ VU.fromList [x, y, 0]

vec4 :: V2 Int -> Vec 4
vec4 (V2 x y) = Vec $ VU.fromList [x, y, 0, 0]

neighbours :: Vec n -> [Vec n]
neighbours (Vec vec) = do
    delta <- VU.replicateM (VU.length vec) [-1, 0, 1]
    guard . not $ VU.all (== 0) delta
    pure . Vec $ VU.zipWith (+) vec delta

step :: Cubes n -> Cubes n
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
    let grid2d = Map.keysSet . Map.filter (== '#') $ Grid.fromString inputstr in
    ( Set.size $ iterate step (Set.map vec3 grid2d) !! 6
    , Set.size $ iterate step (Set.map vec4 grid2d) !! 6
    )
