{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.KnotHash
    ( knotHash
    , knotHashHex

    , single
    ) where

import           Control.Monad               (foldM, forM_)
import           Control.Monad.ST            (ST, runST)
import           Data.Bits                   (xor)
import           Data.Char                   (ord)
import           Data.Proxy                  (Proxy (..))
import           Data.Semigroup              (Semigroup (..))
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word                   (Word8)
import           GHC.TypeLits                (KnownNat, Nat, natVal)
import           Prelude                     hiding (round)
import           Text.Printf                 (printf)

--------------------------------------------------------------------------------

newtype RingIdx (n :: Nat) = RingIdx {fromRingIdx :: Int} deriving (Show)

toRingIdx :: forall n. KnownNat n => Int -> RingIdx n
toRingIdx x = RingIdx (x `mod` fromIntegral (natVal (Proxy :: Proxy n)))

instance forall n. KnownNat n => Semigroup (RingIdx n) where
    RingIdx x <> RingIdx y = toRingIdx (x + y)

instance forall n. KnownNat n => Monoid (RingIdx n) where
    mempty  = RingIdx 0
    mappend = (<>)

--------------------------------------------------------------------------------

data Rope n s a = Rope
    { rSkipSize :: !(RingIdx n)
    , rPosition :: !(RingIdx n)
    , rVec      :: !(VUM.MVector s a)
    }

sequential
    :: forall n s a. (KnownNat n, Num a, VUM.Unbox a) => ST s (Rope n s a)
sequential = do
    let !len = fromIntegral (natVal (Proxy :: Proxy n))
    vec <- VUM.unsafeNew len
    forM_ [0 .. len - 1] $ \i -> VUM.unsafeWrite vec i (fromIntegral i)
    return Rope {rSkipSize = mempty, rPosition = mempty, rVec = vec}

knot :: (KnownNat n, VUM.Unbox a) => Int -> Rope n s a -> ST s (Rope n s a)
knot len rope = do
    forM_ [0 .. (len `div` 2) - 1] $ \i ->
        let !ix = rPosition rope <> toRingIdx i
            !iy = rPosition rope <> toRingIdx (len - 1 - i) in
        VUM.unsafeSwap (rVec rope) (fromRingIdx ix) (fromRingIdx iy)

    return rope
        { rPosition = rPosition rope <> toRingIdx len <> rSkipSize rope
        , rSkipSize = rSkipSize rope <> toRingIdx 1
        }

round :: (KnownNat n, VUM.Unbox a) => [Int] -> Rope n s a -> ST s (Rope n s a)
round lens rope = foldM (\r l -> knot l r) rope lens

densify256 :: VU.Vector Word8 -> VU.Vector Word8
densify256 vec256 = VU.generate 16 $ \i ->
    let slice = VU.slice (i * 16) 16 vec256 in VU.foldl1' xor slice

knotHash :: String -> VU.Vector Word8
knotHash input =
    let !lens2 = map ord input ++ [17, 31, 73, 47, 23]
        !vec2  = runST (run64 (Proxy :: Proxy 256) lens2) in
     densify256 vec2
  where
    run64 :: forall s n. KnownNat n => Proxy n -> [Int] -> ST s (VU.Vector Word8)
    run64 _proxy lens = do
        rope0 <- sequential :: ST s (Rope n s Word8)
        rope1 <- foldM (\r _i -> round lens r) rope0 [0 .. 63 :: Int]
        VU.freeze $ rVec rope1

knotHashHex :: String -> String
knotHashHex = concatMap (printf "%02x") . VU.toList . knotHash

-- | Run a single round.
single :: forall s n. KnownNat n => Proxy n -> [Int] -> ST s (VU.Vector Int)
single _proxy lens = do
    rope0 <- sequential :: ST s (Rope n s Int)
    rope1 <- round lens rope0
    VU.freeze $ rVec rope1
