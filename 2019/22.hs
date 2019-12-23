{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified AdventOfCode.Loop as Loop
import qualified Data.List         as L
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Dual (..), Endo (..))
import           Debug.Trace
import           Text.Read         (readMaybe)

type Size    = Integer
type Iso a   = (Endo a, Dual (Endo a))
type Shuffle = Iso Integer

dealIntoNewStack :: Size -> Shuffle
dealIntoNewStack size =
    let rev = \p -> size - p - 1 in (Endo rev, Dual (Endo rev))

cut :: Size -> Integer -> Shuffle
cut size n =
    let cutFront m = \p -> if p < m then size - m + p  else p - m
        cutWork  m = if m >= 0 then cutFront m else cutFront (size + m) in
    (Endo (cutWork (negate n)), Dual (Endo (cutWork n)))

dealWithIncrement :: Size -> Integer -> Either String Shuffle
dealWithIncrement size n = case modInv n size of
    Nothing -> Left "size and n not coprime"
    Just i  -> pure
        ( Endo $ \p -> (p * i) `mod` size
        , Dual $ Endo $ \p -> (n * p) `mod` size
        )

modInv :: Integer -> Integer -> Maybe Integer
modInv a m
    | 1 == g    = Just (mkPos i)
    | otherwise = Nothing
  where
    (i, _, g) = egcd a m
    mkPos x   = if x < 0 then x + m else x

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (1, 0, a)
egcd a b =
    let (q, r)    = a `quotRem` b
        (s, t, g) = egcd b r in
    (t, s - q * t, g)

parseShuffle :: Size -> String -> Either String Shuffle
parseShuffle size line = case words line of
    ["cut", s] | Just n <- readMaybe s -> pure $ cut size n
    ["deal", "into", "new", "stack"] -> pure $ dealIntoNewStack size
    ["deal", "with", "increment", s] | Just n <- readMaybe s ->
        dealWithIncrement size n
    _ -> Left $ "Could not parse shuffle: " ++ line

parseShuffles :: Size -> String -> Either String [Shuffle]
parseShuffles size = mapM (parseShuffle size) . lines

main :: IO ()
main = do
    input    <- getContents
    shuffle1 <- either fail (pure . mconcat) $ parseShuffles 10007 input
    print $ appEndo (getDual (snd shuffle1)) 2019

    let bigSize = 119315717514047
        times   = 101741582076661
    shuffle2 <- either fail (pure . mconcat) $ parseShuffles bigSize input
    print "TODO"
