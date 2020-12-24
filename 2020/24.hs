{-# LANGUAGE LambdaCase #-}
import qualified AdventOfCode.Hex  as Hex
import           AdventOfCode.Main (pureMain)
import           Data.List         (foldl')
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

parseDirections :: String -> Either String [Hex.Dir]
parseDirections = \case
    's' : 'e' : r -> (Hex.NE :) <$> parseDirections r
    's' : 'w' : r -> (Hex.SE :) <$> parseDirections r
    'n' : 'e' : r -> (Hex.NW :) <$> parseDirections r
    'n' : 'w' : r -> (Hex.SW :) <$> parseDirections r
    'e' :       r -> (Hex.N  :) <$> parseDirections r
    'w' :       r -> (Hex.S  :) <$> parseDirections r
    c   :       _ -> Left $ "Parse fail: unexpected " ++ show c
    []            -> pure []

step :: Set Hex.Cubic -> Set Hex.Cubic
step black = Set.filter isBlack . Set.fromList . concatMap Hex.neighbours $
    Set.toList black
  where
    isBlack pos =
        let c = length . filter (`Set.member` black) $ Hex.neighbours pos in
        if pos `Set.member` black then c == 1 || c == 2 else c == 2

main :: IO ()
main = pureMain $ \inputstr -> do
    directions <- traverse parseDirections $ lines inputstr
    let tiles = map (foldl' (flip Hex.move) Hex.zero) directions
        freqs = Map.fromListWith (+) [(t, 1 :: Int) | t <- tiles]
        black = Map.keysSet $ Map.filter odd freqs
    pure (pure $ Set.size black, pure . Set.size $ iterate step black !! 100)
