module Main where

import           Data.Function      (on)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified System.IO          as IO

type Forms a = NE.NonEmpty (Set.Set a)

readForms :: IO.Handle -> IO [Forms Char]
readForms h =
    fmap (fmap Set.fromList) . filter (/= ("" :| [])) .
    NE.groupBy (on (&&) (not . null)) . lines <$> IO.hGetContents h

intersections :: Ord a => NonEmpty (Set.Set a) -> Set a
intersections (x :| xs) = foldl' Set.intersection x xs

main :: IO ()
main = do
    forms <- readForms IO.stdin
    print . sum $ map (Set.size . Set.unions) forms
    print . sum $ map (Set.size . intersections) forms
