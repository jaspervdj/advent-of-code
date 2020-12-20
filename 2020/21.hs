import           AdventOfCode.Main (simpleMain)
import           Control.Monad     (guard)
import           Data.Char         (isPunctuation)
import           Data.List         (foldl', intercalate, sortOn)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

newtype Ingredient = Ingredient String deriving (Eq, Ord, Show)
newtype Allergen = Allergen String deriving (Eq, Ord, Show)

data Food = Food
    { foodIngredients :: !(Set Ingredient)
    , foodAllergens   :: !(Set Allergen)
    } deriving (Show)

parseMenu :: String -> [Food]
parseMenu = map parseFood . lines . filter (not . isPunctuation)
  where
    parseFood l =
        let (ingr, allg) = break (== "contains") $ words l in
        Food (Set.fromList $ map Ingredient ingr) .
            Set.fromList . map Allergen $ drop 1 allg

intersections :: Ord a => [Set a] -> Set a
intersections []       = Set.empty
intersections (x : xs) = foldl' Set.intersection x xs

eliminate :: Ingredient -> Allergen -> [Food] -> [Food]
eliminate ingr allg foods = do
    Food ingrs allgs <- foods
    let allgs' = Set.delete allg allgs
    guard . not $ Set.null allgs'
    pure $ Food (Set.delete ingr ingrs) allgs'

solve :: [Food] -> [Map Ingredient Allergen]
solve [] = pure Map.empty
solve foods = do
    ag <- Set.toList . Set.unions $ map foodAllergens foods
    let igs = intersections [ig | Food ig ags <- foods, ag `Set.member` ags]
    case Set.toList igs of
        [ig] -> [Map.insert ig ag sol | sol <- solve $ eliminate ig ag foods]
        _    -> pure Map.empty

main :: IO ()
main = simpleMain $ \inputstr ->
    let menu = parseMenu inputstr
        bad = Map.unions $ solve menu in
    ( length [i | Food is _ <- menu, i <- Set.toList is, i `Map.notMember` bad]
    , intercalate "," [ig | (Ingredient ig, _) <- sortOn snd $ Map.toList bad]
    )
