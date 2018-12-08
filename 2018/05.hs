{-# LANGUAGE DeriveFunctor #-}

import Data.Char (toLower)
import qualified Data.Set as S

data Zippy a = Zippy [a] [a]
    deriving (Functor, Show)

fromList :: [a] -> Zippy a
fromList = Zippy []

reacts :: Char -> Char -> Bool
reacts x y = x /= y && toLower x == toLower y

run :: (a -> a -> Bool) -> Zippy a -> [a]
run _ (Zippy rev      [])       = reverse rev
run f (Zippy []       (x : xs)) = run f (Zippy [x] xs)
run f (Zippy (y : ys) (x : xs))
    | f y x     = run f (Zippy ys xs)
    | otherwise = run f (Zippy (x : y : ys) xs)

main :: IO ()
main = do
    input <- getLine

    let final = run reacts $ fromList input
    print $ length final

    let chars = S.fromList $ map toLower input
    print $ minimum $ do
        c <- S.toList chars
        return $ length $ run reacts $ fromList $
            filter (\x -> toLower x /= c) input
