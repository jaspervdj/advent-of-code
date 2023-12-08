{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
import qualified Data.Foldable       as F
import           Data.Foldable.Extra (minimumOn)
import qualified Data.List           as L
import           Data.Maybe          (mapMaybe)

newtype Compose f g a = Compose (f (g a))

deriving instance (Functor     f, Functor     g) => Functor     (Compose f g)
deriving instance (Foldable    f, Foldable    g) => Foldable    (Compose f g)
deriving instance (Traversable f, Traversable g) => Traversable (Compose f g)

count :: (Eq a, Foldable f) => a -> f a -> Int
count x = F.foldl' (\acc y -> if x == y then acc + 1 else acc) 0

cut :: Int -> [a] -> [[a]]
cut n xs = let (y, ys) = splitAt n xs in if null y then [] else y : cut n ys

data Pixel = Black | White | Transparent deriving (Eq, Show)

instance Semigroup Pixel where
    Transparent <> y = y
    x           <> _ = x

instance Monoid Pixel where
    mempty = Transparent

parsePixel :: Char -> Maybe Pixel
parsePixel = \case
    '0' -> Just Black
    '1' -> Just White
    '2' -> Just Transparent
    _   -> Nothing

renderPixel :: Pixel -> Char
renderPixel Black       = ' '
renderPixel White       = 'M'
renderPixel Transparent = '-'

main :: IO ()
main = do
    img <- map (cut 25) . cut (25 * 6) . mapMaybe parsePixel <$> getContents

    let layer0 = minimumOn (count Black) (map Compose img)
    print $ count White layer0 * count Transparent layer0

    putStr $ unlines $ map (map renderPixel) $
        L.foldl' (zipWith (zipWith (<>))) (repeat $ repeat Transparent) img
