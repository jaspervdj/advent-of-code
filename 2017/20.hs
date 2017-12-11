{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
import           Data.Char           (isDigit)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (mapMaybe)
import           Data.Ord            (comparing)
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import qualified System.IO           as IO
import           Text.Read           (readMaybe)

data V3 = V3
    { vX :: {-# UNPACK #-} !Int
    , vY :: {-# UNPACK #-} !Int
    , vZ :: {-# UNPACK #-} !Int
    } deriving (Eq, Generic)

instance Hashable V3

(.+.) :: V3 -> V3 -> V3
V3 x1 y1 z1 .+. V3 x2 y2 z2 = V3 (x1 + x2) (y1 + y2) (z1 + z2)
infixl 6 .+.

manhattan :: V3 -> Int
manhattan v = abs (vX v) + abs (vY v) + abs (vZ v)

data Particle = Particle
    { pPos :: {-# UNPACK #-} !V3
    , pVel :: {-# UNPACK #-} !V3
    , pAcc :: {-# UNPACK #-} !V3
    } deriving (Eq, Generic)

stepParticle :: Particle -> Particle
stepParticle p =
    let !vel = pVel p .+. pAcc p
        !pos = pPos p .+. vel in
    p {pVel = vel, pPos = pos}

stepParticles :: V.Vector Particle -> V.Vector Particle
stepParticles particles =
    let collisions = V.foldl'
            (\acc p -> HMS.insertWith (+) (pPos p) (1 :: Int) acc)
            HMS.empty particles in
    V.map stepParticle $ V.filter
        (\p -> maybe False (<= 1) $ HMS.lookup (pPos p) collisions)
        particles

iterateParticles :: Int -> V.Vector Particle -> V.Vector Particle
iterateParticles n particles
    | n <= 0    = particles
    | otherwise = iterateParticles (n - 1) (stepParticles particles)

readParticles :: IO.Handle -> IO (V.Vector Particle)
readParticles h = do
    ls <- lines <$> IO.hGetContents h
    V.fromList <$> mapM parseParticle ls
  where
    parseParticle line =
        let good   = \c -> isDigit c || c == '-'
            spaced = map (\c -> if good c then c else ' ') line
            ints   = mapMaybe readMaybe (words spaced) in
        case ints of
            [px, py, pz, vx, vy, vz, ax, ay, az] -> return $
                Particle (V3 px py pz) (V3 vx vy vz) (V3 ax ay az)
            _ -> fail $ "Could not parse particle: " ++ show line

main :: IO ()
main = do
    particles <- readParticles IO.stdin
    -- For the first problem we really just want the particle with the lowest
    -- acceleration.
    let key p   = (manhattan (pAcc p), manhattan (pVel p), manhattan (pPos p))
        closest = V.minIndexBy (comparing key) particles
    putStrLn $ "Runaway: " ++ show closest

    let iterations = 10000
        final      = iterateParticles iterations particles
    putStrLn $ "Particles left: " ++ show (V.length final)
