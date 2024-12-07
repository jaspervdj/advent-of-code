import qualified AdventOfCode.Dijkstra    as Dijkstra
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser  as NP
import           AdventOfCode.V3          (V3 (..))
import qualified AdventOfCode.V3          as V3
import           Control.Monad            (guard)
import           Data.Foldable            (toList)
import           Data.List.NonEmpty.Extra (NonEmpty, foldl1')
import qualified Data.Map                 as M
import qualified Data.Set                 as S

newtype Voxel = Voxel {unVoxel :: V3 Int} deriving (Eq, Ord)
newtype Face  = Face  (V3 Int) deriving (Eq, Ord)

parseInput :: NP.Parser Char (NonEmpty Voxel)
parseInput = fmap (fmap Voxel) $ v3 `NP.sepBy1` NP.newline
  where
    v3 = V3
        <$> NP.signedDecimal
        <*> (NP.char ',' *> NP.signedDecimal)
        <*> (NP.char ',' *> NP.signedDecimal)

voxelFaces :: Voxel -> [Face]
voxelFaces (Voxel (V3 x y z)) = map Face
    [ V3 (x * 2 + 1) (y * 2 + 1) (z * 2    )
    , V3 (x * 2    ) (y * 2 + 1) (z * 2 + 1)
    , V3 (x * 2 + 1) (y * 2)     (z * 2 + 1)
    , V3 (x * 2 + 1) (y * 2 + 1) (z * 2 + 2)
    , V3 (x * 2 + 2) (y * 2 + 1) (z * 2 + 1)
    , V3 (x * 2 + 1) (y * 2 + 2) (z * 2 + 1)
    ]

voxelNeighbours :: Voxel -> [Voxel]
voxelNeighbours (Voxel (V3 x y z)) = map Voxel
    [ V3 (x - 1)  y       z
    , V3 (x + 1)  y       z
    , V3  x      (y - 1)  z
    , V3  x      (y + 1)  z
    , V3  x       y      (z - 1)
    , V3  x       y      (z + 1)
    ]

part1 :: NonEmpty Voxel -> Int
part1 cubes = M.size . M.filter (== 1) $
    M.fromListWith (+) [(p, 1 :: Int) | c <- toList cubes, p <- voxelFaces c]

part2 :: NonEmpty Voxel -> Int
part2 voxels = S.size $
    (S.fromList $ steam  >>= voxelFaces) `S.intersection`
    (S.fromList $ toList voxels >>= voxelFaces)
  where
    droplet = S.fromList $ toList voxels
    steam   = M.keys . Dijkstra.bfsDistances $ Dijkstra.bfs
        (\voxel -> do
            neighbour@(Voxel (V3 x y z)) <- voxelNeighbours voxel
            guard . not $ S.member neighbour droplet
            guard $
                x >= minX - 1 && x <= maxX + 1 &&
                y >= minY - 1 && y <= maxY + 1 &&
                z >= minZ - 1 && z <= maxZ + 1
            pure neighbour)
        (const False)
        (Voxel (V3 minX minY (minZ - 1)))

    V3 minX minY minZ = foldl1' (V3.zipWith min) $ fmap unVoxel voxels
    V3 maxX maxY maxZ = foldl1' (V3.zipWith max) $ fmap unVoxel voxels

main :: IO ()
main = pureMain $ \input -> do
    voxels <- NP.runParser parseInput input
    pure (pure (part1 voxels), pure (part2 voxels))
