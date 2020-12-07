{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Monad (guard)
import           Data.List     (isPrefixOf)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import qualified Data.Set      as Set
import           Text.Read     (readMaybe)

data Bag = Bag String String deriving (Eq, Ord, Show)

type Rules = [(Bag, [(Int, Bag)])]

parseRules :: String -> Maybe Rules
parseRules = traverse parseBagLine . lines
  where
    parseBagLine :: String -> Maybe (Bag, [(Int, Bag)])
    parseBagLine line = case words line of
        c1 : c2 : "bags" : "contain" : rest ->
            (Bag c1 c2, ) <$> parseConstraints rest
        _ -> Nothing

    parseConstraints = \case
        [] -> Just []
        ["no", "other", "bags."] -> Just []
        n : c1 : c2 : b : rest | Just x <- readMaybe n , "bag" `isPrefixOf` b ->
            ((x, Bag c1 c2) :) <$> parseConstraints rest
        _ -> Nothing

containing :: Bag -> Rules -> [Bag]
containing target rules =
    Set.toList $ go Set.empty [target]
  where
    rev = Map.fromListWith (++) [(y, [x]) | (x, ys) <- rules, (_, y) <- ys]
    go visited [] = visited
    go visited fringe =
        let visited' = visited <> Set.fromList fringe
            fringe' = do
                f <- fringe
                n <- fromMaybe [] $ Map.lookup f rev
                guard . not $ n `Set.member` visited'
                pure n in
        go visited' fringe'

contained :: Bag -> Rules -> Int
contained target rules =
    let get b = fromMaybe 0 $ Map.lookup b lazy
        lazy = Map.fromList $ do
            (x, ys) <- rules
            pure (x, sum [n * (1 + get y) | (n, y) <- ys]) in
    get target

main :: IO ()
main = do
    rules <- getContents >>= maybe (fail "parse") pure . parseRules
    print $ length (containing (Bag "shiny" "gold") rules) - 1
    print $ contained (Bag "shiny" "gold") rules
