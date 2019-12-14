{-# LANGUAGE DeriveFoldable #-}
module Main where

import qualified AdventOfCode.NanoParser as NP
import qualified Data.Foldable           as F
import qualified Data.Graph              as G
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as Set
import qualified System.IO               as IO

type Chemical = String
data Reaction a = Reaction [(Int, a)] (Int, a) deriving (Foldable, Show)

parseReactions :: NP.Parser Char [Reaction Chemical]
parseReactions = NP.many1 $ Reaction
    <$> NP.sepBy1 quantity (NP.char ',' <* NP.spaces)
    <*  NP.string "=>" <* NP.spaces
    <*> quantity
  where
    quantity = (,)
        <$> NP.decimal <* NP.spaces
        <*> NP.many1 NP.alpha <* NP.spaces

reverseSearch :: Ord a => [Reaction a] -> Map.Map a Int -> Map.Map a Int
reverseSearch reactions requirements0 = F.foldl'
    (\requirements element -> case Map.lookup element reactionsByRhs of
        Nothing -> requirements  -- We probably arrived at ORE
        Just (Reaction inputs (quantity, _)) ->
            let required = fromMaybe 0 $ Map.lookup element requirements
                times = (required + quantity - 1) `div` quantity in
            Map.unionWith (+) requirements $
                Map.fromList [(e, q * times) | (q, e) <- inputs])
    requirements0
    ordered
  where
    edges c  = [to | Reaction from (_, to) <- reactions, c `elem` map snd from]
    elements = Set.fromList $ concatMap F.toList reactions
    ordered  = concatMap G.flattenSCC $ G.stronglyConnComp
        [(c, c, edges c) | c <- Set.toList elements]

    reactionsByRhs = Map.fromList
        [(to, r) | r@(Reaction _ (_, to)) <- reactions]

main :: IO ()
main = do
    reactions <- NP.hRunParser IO.stdin parseReactions
    print . fromMaybe 0 . Map.lookup "ORE" $
        reverseSearch reactions (Map.singleton "FUEL" 1)
