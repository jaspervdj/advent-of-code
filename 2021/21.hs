{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           AdventOfCode.Main (simpleMain)
import           Control.Monad     (replicateM)
import           Data.Bifunctor    (first)
import           Data.Foldable     (fold)
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Sum (..))

--------------------------------------------------------------------------------

wrap1 :: Int -> Int -> Int
wrap1 n x = ((x - 1) `mod` n) + 1

data DiceState = DiceState
    { diceNext  :: !Int
    , diceRolls :: !Int
    } deriving (Show)

mkDiceState :: DiceState
mkDiceState = DiceState 1 0

rollDice :: DiceState -> (Int, DiceState)
rollDice DiceState {..} =
    (diceNext, DiceState (wrap1 100 (diceNext + 1)) (diceRolls + 1))

rollDiceN :: Int -> DiceState -> (Int, DiceState)
rollDiceN n dice =
    let (x, dice') = rollDice dice in
    if n <= 1 then (x, dice') else first (+ x) $ rollDiceN (n - 1) dice'

data PlayerState = PlayerState
    { playerScore :: !Int
    , playerPlace :: !Int
    } deriving (Show)

mkPlayerState :: Int -> PlayerState
mkPlayerState = PlayerState 0

takeTurn :: DiceState -> PlayerState -> (DiceState, PlayerState)
takeTurn dice PlayerState {..} =
    let (move, dice') = rollDiceN 3 dice
        place         = wrap1 10 $ playerPlace + move in
    (dice', PlayerState (playerScore + place) place)

playGame :: DiceState -> PlayerState -> PlayerState -> (DiceState, PlayerState, PlayerState)
playGame dice player1 player2 =
    let (dice', player1') = takeTurn dice player1 in
    if playerScore player1' >= 1000 then
        (dice', player1', player2)
    else
        playGame dice' player2 player1'

--------------------------------------------------------------------------------

type Future = (Int, Int, Int)

dice3 :: [(Int, Integer)]
dice3 = M.toList $ M.fromListWith (+) [(sum n, 1) | n <- replicateM 3 [1, 2, 3]]

futures :: Int -> M.Map Future Integer
futures initialPos = memo
  where
    -- After 7 turns someone should def win?
    keys = (,,) <$> [0 .. 20] <*> [1 .. 10] <*> [0 .. 31]

    memo = M.fromList [(k, combinations k) | k <- keys]

    combinations (trn, pos, pts)
        | pts - pos >= 21 = 0  -- Already won
        | trn == 0        = if pos == initialPos && pts == 0 then 1 else 0
        | otherwise       = sum $ do
            (move, c) <- dice3
            let pts' = pts - pos
                trn' = trn - 1
                pos' = wrap1 10 (pos - move)
            pure $ (c *) $ fromMaybe 0 $ M.lookup (trn', pos', pts') memo

data Both a = Both !a !a deriving (Foldable, Functor, Show)

instance Semigroup a => Semigroup (Both a) where
    Both x1 y1 <> Both x2 y2 = Both (x1 <> x2) (y1 <> y2)

instance Monoid a => Monoid (Both a) where
    mempty = Both mempty mempty

wins :: Int -> Int -> Both (Sum Integer)
wins pos1 pos2 = fold $ M.intersectionWith
    (\pts1 pts2 -> mconcat $ do
        (pt1, c1) <- M.toList pts1
        (pt2, c2) <- M.toList pts2
        -- Player 2 does need to reach this turn, so remove possibilities
        let c2' = div c2 . sum $ map snd dice3
        pure $
            if | pt1 >= 21 -> Both (Sum $ c1 * c2') mempty
               | pt2 >= 21 -> Both mempty (Sum $ c1 * c2)
               | otherwise -> Both mempty mempty)
    (byTurn $ futures pos1)
    (byTurn $ futures pos2)
  where
    byTurn futs = M.fromListWith (M.unionWith (+)) $ do
        ((turn, _, pts), c) <- M.toList futs
        pure (turn, M.singleton pts c)

--------------------------------------------------------------------------------

main :: IO ()
main = simpleMain $ \input ->
    let [pos1, pos2] = map (read . last . words) $ lines input
        (dice, _, loser) =
            playGame mkDiceState (mkPlayerState pos1) (mkPlayerState pos2)
        part1 = diceRolls dice * playerScore loser in
    (part1, maximum $ getSum <$> wins pos1 pos2)
