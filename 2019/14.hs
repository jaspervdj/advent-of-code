{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified AdventOfCode.BinarySearch as BS
import qualified AdventOfCode.NanoParser   as NP
import           Control.Monad             (forM_, guard)
import           Control.Monad.Except      (throwError)
import           Control.Monad.Reader      (ReaderT, asks, runReaderT)
import           Control.Monad.State       (StateT, execStateT, modify, state)
import           Data.Either               (isRight)
import           Data.Functor              (($>))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import qualified System.IO                 as IO

data Recipe     a = Recipe [(Int, a)] (Int, a) deriving (Foldable, Show)
type RecipeBook a = Map.Map a (Recipe a)
type Inventory  a = Map.Map a Int

parseRecipeBook :: NP.Parser Char [Recipe String]
parseRecipeBook = NP.many1 $ Recipe
    <$> NP.sepBy1 quantity (NP.char ',' <* NP.spaces)
    <*  NP.string "=>" <* NP.spaces
    <*> quantity
  where
    quantity = (,)
        <$> NP.decimal <* NP.spaces
        <*> NP.many1 NP.alpha <* NP.spaces

makeRecipeBook :: Ord a => [Recipe a] -> RecipeBook a
makeRecipeBook rs = Map.fromList [(to, r) | r@(Recipe _ (_, to)) <- rs]

type MixerM e a = ReaderT (RecipeBook e) (StateT (Inventory e) (Either e)) a

mix :: Ord a => Int -> a -> MixerM a ()
mix want x = do
    got <- state $ \inv -> case Map.lookup x inv of
        Nothing  -> (0, inv)
        Just got ->
            let n = max 0 (min want got) in (n, Map.insert x (got - n) inv)

    recipe <- asks (Map.lookup x)
    case recipe of
        _ | got >= want                    -> pure ()
        Nothing                              -> throwError x
        Just (Recipe inputs (quantity, _)) -> do
            let required = want - got
                times    = (required + quantity - 1) `div` quantity
                leftover = times * quantity - required
            forM_ inputs $ \(inputq, inpute) -> mix (inputq * times) inpute
            modify $ Map.insertWith (+) x leftover

main :: IO ()
main = do
    recipes <- makeRecipeBook <$> NP.hRunParser IO.stdin parseRecipeBook
    let ores   = 1000000000000
        cargo  = Map.singleton "ORE" ores
        fuel n = execStateT (runReaderT (mix n "FUEL") recipes) cargo
    either fail (print . maybe 0 (ores -) . Map.lookup "ORE") (fuel 1)
    print . fromMaybe 0 . BS.upperBound $ \n -> guard (isRight $ fuel n) $> n
