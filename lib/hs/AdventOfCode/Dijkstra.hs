{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.Dijkstra
    ( Find (..)
    , Options (..)
    , defaultOptions
    , Result (..)
    , dijkstra
    , distances
    , backtrack
    ) where

import qualified AdventOfCode.PriorityQueue as PQ
import           Control.Monad              (guard)
import           Data.Foldable              (toList)
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, listToMaybe)
import qualified Data.Set                   as S

data Find v
    = FindAll (v -> Bool)
    | FindOne (v -> Bool)
    | NoFind

data Options v = Options
    { neighbours :: !(v -> [(Int, v)])
    , start      :: !(S.Set v)
    , find       :: !(Find v)
    }

defaultOptions :: Options v
defaultOptions = Options
    { neighbours = const []
    , start      = S.empty
    , find       = NoFind
    }

data Result v = Result
    { goal :: Maybe (Int, S.Set v)
    , back :: Map.Map v (Int, S.Set v)
    }

dijkstra :: forall v. Ord v => Options v -> Result v
dijkstra opts = go
    (PQ.fromList [(0, (v, Nothing)) | v <- toList $ start opts])
    Nothing
    (Map.fromList [(v, (0, S.empty)) | v <- toList $ start opts])
  where
    go  :: PQ.PriorityQueue Int (v, Maybe v)
        -> Maybe (Int, S.Set v)
        -> Map.Map v (Int, S.Set v)
        -> Result v
    go fringe0 mbBest0 bt0 = case PQ.pop fringe0 of
        _ | Just _ <- mbBest0, FindOne _ <- find opts -> Result mbBest0 bt0
        Nothing -> Result mbBest0 bt0
        Just (cdist, (current, mbPrev), fringe1)
            | not (current `S.member` start opts)
            , FindOne _ <- find opts
            , current `Map.member` bt0 ->
                go fringe1 mbBest0 bt0
            | not (current `S.member` start opts)
            , NoFind <- find opts
            , current `Map.member` bt0 ->
                go fringe1 mbBest0 bt0
            | maybe False (\(d, _) -> cdist > d) (Map.lookup current bt0) ->
                go fringe1 mbBest0 bt0
            | maybe False ((< cdist) . fst) mbBest0 -> case find opts of
                NoFind -> go fringe1 mbBest0 bt0  -- Continue looking always
                _      -> Result mbBest0 bt0
            | otherwise ->
                let interesting = do
                        (d, neighbour) <- neighbours opts current
                        guard $ not $ neighbour `Map.member` bt0
                        pure (cdist + d, neighbour)

                    bt1 = case mbPrev of
                        Nothing -> bt0
                        Just prev -> Map.insertWith
                            lowest current (cdist, S.singleton prev) bt0

                    fringe2 = L.foldl'
                        (\acc (d, v) -> PQ.push d (v, Just current) acc)
                        fringe1 interesting

                    mbBest1
                        | goal current = Just $ maybe
                            (cdist, S.singleton current)
                            (lowest (cdist, S.singleton current))
                            mbBest0
                        | otherwise    = mbBest0 in

                go fringe2 mbBest1 bt1

    goal node = case find opts of
        FindAll p -> p node
        FindOne p -> p node
        NoFind    -> False

    lowest :: (Int, S.Set v) -> (Int, S.Set v) -> (Int, S.Set v)
    lowest (c1, p1) (c2, p2)
            | c1 < c2   = (c1, p1)
            | c1 > c2   = (c2, p2)
            | otherwise = (c1, p1 <> p2)

distances :: Result v -> Map.Map v Int
distances = fmap fst . back

backtrack :: forall v. Ord v => v -> Result v -> [v]
backtrack end (Result _ b) = go [end] end
  where
    go :: [v] -> v -> [v]
    go acc node = fromMaybe acc $ do
        (_, prev) <- Map.lookup node b
        p <- listToMaybe $ S.toList prev
        pure $ go (p : acc) p
