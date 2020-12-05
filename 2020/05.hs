{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Main where

import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Control.Monad           (replicateM)
import           Data.List               (foldl')
import           Data.Maybe              (listToMaybe)
import qualified Data.Set                as Set
import qualified System.IO               as IO

data Bit = Low | High deriving (Eq, Show)

partition :: [Bit] -> Int
partition bits = fst $ foldl' pick (0, 2 ^ length bits) bits
  where
    pick (lo, up) = \case
        Low  -> (lo, mid)
        High -> (mid, up)
      where
        !mid = (lo + up) `div` 2

data Seat = Seat [Bit] [Bit] deriving (Show)

parseSeats :: NP.Parser Char [Seat]
parseSeats = NP.sepBy seat (NP.char '\n')
  where
    seat = Seat <$> replicateM 7 fb <*> replicateM 3 lr
    fb   = (Low <$ NP.char 'F') <|> (High <$ NP.char 'B')
    lr   = (Low <$ NP.char 'L') <|> (High <$ NP.char 'R')

seatId :: Seat -> Int
seatId (Seat fb lr) = partition fb * 8 + partition lr

main :: IO ()
main = do
    seats <- NP.hRunParser IO.stdin parseSeats
    let taken = Set.fromList $ map seatId seats
    print $ maximum taken
    maybe (fail "no solution") print $ listToMaybe
        [ x
        | x <- [minimum taken .. maximum taken]
        , not $ x `Set.member` taken
        , pred x `Set.member` taken && succ x `Set.member` taken
        ]
