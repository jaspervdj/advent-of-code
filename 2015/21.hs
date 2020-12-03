module Main where

import           Data.Either        (partitionEithers)
import           Data.List.Extended (maximumOn, minimumOn, selectN)

data Combatant = Combatant
    { cHitpoints :: !Int
    , cDamage    :: !Int
    , cArmor     :: !Int
    } deriving (Show)

simulate :: Combatant -> Combatant -> (Bool, Combatant, Combatant)
simulate = go True
  where
    go _ cmb1 cmb2 | cHitpoints cmb1 <= 0 = (False, cmb1, cmb2)
    go _ cmb1 cmb2 | cHitpoints cmb2 <= 0 = (True, cmb1, cmb2)
    go True cmb1 cmb2 = go False cmb1 (attack cmb1 cmb2)
    go False cmb1 cmb2 = go True (attack cmb2 cmb1) cmb2

    attack attacker defender =
        let dmg = max 1 $ cDamage attacker - cArmor defender in
        defender {cHitpoints = cHitpoints defender - dmg}

data Inventory = Inventory
    { iItems  :: [String]
    , iCost   :: !Int
    , iDamage :: !Int
    , iArmor  :: !Int
    } deriving (Show)

instance Semigroup Inventory where
    x <> y = Inventory
        { iItems = iItems x ++ iItems y
        , iCost = iCost x + iCost y
        , iDamage = iDamage x + iDamage y
        , iArmor = iArmor x + iArmor y
        }

instance Monoid Inventory where
    mempty = Inventory [] 0 0 0

inventoryToCombatant :: Int -> Inventory -> Combatant
inventoryToCombatant hitpoints inventory = Combatant
    { cHitpoints = hitpoints
    , cDamage    = iDamage inventory
    , cArmor     = iArmor inventory
    }

weapons :: [Inventory]
weapons =
    [ Inventory ["Dagger"]        8     4       0
    , Inventory ["Shortsword"]   10     5       0
    , Inventory ["Warhammer"]    25     6       0
    , Inventory ["Longsword"]    40     7       0
    , Inventory ["Greataxe"]     74     8       0
    ]

armor :: [Inventory]
armor =
    [ Inventory ["Leather"]      13     0       1
    , Inventory ["Chainmail"]    31     0       2
    , Inventory ["Splintmail"]   53     0       3
    , Inventory ["Bandedmail"]   75     0       4
    , Inventory ["Platemail"]   102     0       5
    ]

rings :: [Inventory]
rings =
    [ Inventory ["Damage +1"]    25     1       0
    , Inventory ["Damage +2"]    50     2       0
    , Inventory ["Damage +3"]   100     3       0
    , Inventory ["Defense +1"]   20     0       1
    , Inventory ["Defense +2"]   40     0       2
    , Inventory ["Defense +3"]   80     0       3
    ]

shop :: [Inventory]
shop = do
    weapon <- weapons
    armor' <- mempty : armor
    numRings <- [0 .. 2]
    (rings', _) <- selectN numRings rings
    pure $ weapon <> armor' <> mconcat rings'

main :: IO ()
main = do
    let (wins, losses) = partitionEithers $ do
            inventory <- shop
            let player = inventoryToCombatant 100 inventory
                boss = Combatant 100 8 2
                (won, _, _) = simulate player boss
            pure $ if won then Left inventory else Right inventory
    print . iCost $ minimumOn iCost wins
    print . iCost $ maximumOn iCost losses
