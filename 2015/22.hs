{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           AdventOfCode.Dijkstra (dijkstra)
import           Control.Monad         ((<=<))
import           Data.Either           (isLeft)
import qualified Data.Map              as Map
import           Data.Maybe            (maybeToList)

data Wizard = Wizard
    { wHitpoints :: !Int
    , wMana      :: !Int
    , wArmor     :: !Int
    , wShield    :: !Int
    , wPoison    :: !Int
    , wRecharge  :: !Int
    } deriving (Eq, Ord, Show)

data Spell
    = MagicMissile
    | Drain
    | Shield
    | Poison
    | Recharge
    deriving (Bounded, Enum, Show)

data Boss = Boss
    { bHitpoints :: !Int
    , bDamage    :: !Int
    } deriving (Eq, Ord, Show)

data Halt a = Invalid | Win a | Lose a deriving (Show)
type Game a = a -> Either (Halt a) a

makeWizard :: Int -> Int -> Wizard
makeWizard hp mana = Wizard hp mana 0 0 0 0

checkHitpoints :: Game (Wizard, Boss)
checkHitpoints (wizard@Wizard {..}, boss@Boss {..})
    | bHitpoints <= 0 = Left $ Win (wizard, boss)
    | wHitpoints <= 0 = Left $ Lose (wizard, boss)
    | otherwise       = pure (wizard, boss)

applyShield :: Game (Wizard, Boss)
applyShield (wizard@Wizard {..}, boss)
    | wShield <= 0 = pure (wizard {wArmor = 0}, boss)
    | otherwise    = pure (wizard {wArmor = 7, wShield = wShield - 1}, boss)

applyPoison :: Game (Wizard, Boss)
applyPoison (wizard@Wizard {..}, boss@Boss {..})
    | wPoison <= 0 = pure (wizard, boss)
    | otherwise    = checkHitpoints
        (wizard {wPoison = wPoison - 1}, boss {bHitpoints = bHitpoints - 3})

applyRecharge :: Game (Wizard, Boss)
applyRecharge (wizard@Wizard {..}, boss)
    | wRecharge <= 0 = pure (wizard, boss)
    | otherwise      = pure
        (wizard {wRecharge = wRecharge - 1, wMana = wMana + 101}, boss)

applyEffects :: Game (Wizard, Boss)
applyEffects = applyPoison <=< applyRecharge <=< applyShield

spellCost :: Spell -> Int
spellCost = \case
    MagicMissile -> 53
    Drain        -> 73
    Shield       -> 113
    Poison       -> 173
    Recharge     -> 229

castSpell :: Spell -> Game (Wizard, Boss)
castSpell spell (wizard@Wizard {..}, boss)
    | wMana < cost = Left Invalid
    | otherwise    = pure (wizard {wMana = wMana - cost}, boss)
  where
    cost = spellCost spell

applySpell :: Spell -> Game (Wizard, Boss)
applySpell spell (wizard@Wizard {..}, boss@Boss {..}) = case spell of
    MagicMissile -> pure (wizard, boss {bHitpoints = bHitpoints - 4})
    Drain -> checkHitpoints
        ( wizard {wHitpoints = wHitpoints + 2}
        , boss {bHitpoints = bHitpoints - 2}
        )
    Shield | wShield > 0 -> Left Invalid
    Shield -> pure (wizard {wShield = 6}, boss)
    Poison | wPoison > 0 -> Left Invalid
    Poison -> pure (wizard {wPoison = 6}, boss)
    Recharge | wRecharge > 0 -> Left Invalid
    Recharge -> pure (wizard {wRecharge = 5}, boss)

playerTurn :: Spell -> Game (Wizard, Boss)
playerTurn spell = applySpell spell <=< castSpell spell <=< applyEffects

applyAttack :: Game (Wizard, Boss)
applyAttack (wizard@Wizard {..}, boss@Boss{..}) =
    let dmg = max 1 $ bDamage - wArmor in
    checkHitpoints (wizard {wHitpoints = wHitpoints - dmg}, boss)

bossTurn :: Game (Wizard, Boss)
bossTurn = applyAttack <=< applyEffects

hardMode :: Game (Wizard, Boss)
hardMode (wizard@Wizard {..}, boss) =
    checkHitpoints (wizard {wHitpoints = wHitpoints - 1}, boss)

main :: IO ()
main = do
    let toVertex (Left (Win _)) = Just $ Left ()
        toVertex (Left _)       = Nothing
        toVertex (Right state)  = Just $ Right state

        neighbours1 (Left _) = []
        neighbours1 (Right state) = do
            spell <- [minBound .. maxBound]
            v <- maybeToList . toVertex $ playerTurn spell state >>= bossTurn
            pure (spellCost spell, v)

        neighbours2 (Left _) = []
        neighbours2 (Right state) = do
            v' <- maybeToList . toVertex $ hardMode state
            neighbours1 v'

        isGoal = isLeft
        state0 = Right (makeWizard 50 500, Boss 51 9)

        distances1 = Map.toList $ dijkstra neighbours1 isGoal state0
        distances2 = Map.toList $ dijkstra neighbours2 isGoal state0

    print . minimum . map (fst . snd) $ filter (isGoal . fst) distances1
    print . minimum . map (fst . snd) $ filter (isGoal . fst) distances2
