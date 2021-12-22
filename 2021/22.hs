{-# LANGUAGE DeriveFunctor #-}
module Main where

import           AdventOfCode.Main           (simpleMain)
import           AdventOfCode.V3             (V3 (..), (.-.))
import qualified AdventOfCode.V3             as V3
import           Data.Char                   (isDigit)
import           Data.Foldable               (foldl', for_)
import           Data.List                   (isPrefixOf)
import           Data.Maybe                  (maybeToList)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Box a = Box
    { bMin :: !(V3 a)
    , bMax :: !(V3 a)
    } deriving (Functor, Show)

instance Ord a => Semigroup (Box a) where
    Box lo0 hi0 <> Box lo1 hi1 =
        Box (V3.zipWith min lo0 lo1) (V3.zipWith max hi0 hi1)

intersection :: Ord a => Box a -> Box a -> Maybe (Box a)
intersection (Box lo0 hi0) (Box lo1 hi1)
    | v3X hi0 < v3X lo1 || v3X lo0 > v3X hi1 = Nothing
    | v3Y hi0 < v3Y lo1 || v3Y lo0 > v3Y hi1 = Nothing
    | v3Z hi0 < v3Z lo1 || v3Z lo0 > v3Z hi1 = Nothing
    | otherwise                              = Just $ Box
        (V3.zipWith max lo0 lo1) (V3.zipWith min hi0 hi1)

fromV3 :: V3 a -> Box a
fromV3 p = Box p p

toV3s :: Integral a => Box a -> [V3 a]
toV3s (Box (V3 lx ly lz) (V3 hx hy hz)) =
    V3 <$> [lx .. hx] <*> [ly .. hy] <*> [lz .. hz]

width, height, depth :: Integral a => Box a -> a
width  (Box (V3 lx _  _ ) (V3 hx _  _ )) = hx - lx + 1
height (Box (V3 _  ly _ ) (V3 _  hy _ )) = hy - ly + 1
depth  (Box (V3 _  _  lz) (V3 _  _  hz)) = hz - lz + 1

volume :: Integral a => Box a -> a
volume b = width b * height b * depth b

dumb :: Box Int -> [(Bool, Box Int)] -> Int
dumb bounds boxes = VU.length $ VU.filter id vec
  where
    vec = VU.create $ do
        v <- VUM.replicate (volume bounds) False
        for_ boxes $ \(state, box) -> case intersection box bounds of
            Nothing   -> pure ()
            Just itsn -> for_ (toV3s itsn) $ \p -> do
                VUM.write v (idx p) state
        pure v

    idx v =
        let (V3 x y z) = v .-. bMin bounds in
        x + width bounds * (y + depth bounds * z)

smart :: [(Bool, Box Int)] -> Integer
smart = sum . map toVolume .
    foldl' (\acc (on, box) -> if on then insert acc box else remove acc box) []
  where
    insert, remove :: [(Box Int, Bool)] -> Box Int -> [(Box Int, Bool)]
    insert bs box = (box, True) : remove bs box
    remove bs box =
        [(i, not on) | (b, on) <- bs, i <- maybeToList $ intersection b box] ++
        bs

    toVolume (b, on) = (if on then id else negate) . volume $ fromIntegral <$> b

main :: IO ()
main = simpleMain $ \input ->
    let boxes = map parseBox $ lines input
        bounds = Box (V3 (-50) (-50) (-50)) (V3 50 50 50) in
    (dumb bounds boxes, smart boxes)

parseBox :: String -> (Bool, Box Int)
parseBox line =
    let state   = "on" `isPrefixOf` line
        clear c = if isDigit c || c == '-' then c else ' ' in
    case map read . words $ map clear line of
        [x0, x1, y0, y1, z0, z1] ->
            (state, fromV3 (V3 x0 y0 z0) <> fromV3 (V3 x1 y1 z1))
        _ -> error $ "Could not parse line: " ++ show line
