{-# LANGUAGE ScopedTypeVariables #-}
module AdventOfCode.Bfs
    ( Options (..)
    , defaultOptions
    , Result (..)
    , bfs
    , reachable
    , distances
    , backtrack
    ) where

import           Control.Monad (guard)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import qualified Data.Set      as S

data Options v = Options
    { neighbours :: !(v -> [v])
    , find       :: !(v -> Bool)
    , limit      :: !(Maybe Int)
    , start      :: S.Set v
    }

defaultOptions :: Options v
defaultOptions = Options
    { neighbours = const []
    , find       = const False
    , limit      = Nothing
    , start      = S.empty
    }

data Result v = Result
    { back :: Map.Map v (Int, Maybe v)
    , goal :: Maybe (v, Int)
    }

bfs :: Ord v => Options v -> Result v
bfs opts = go 0 initial initial
  where
    initial = Map.fromList [(s, (0, Nothing)) | s <- S.toList (start opts)]

    go !i visited fringe
        | maybe False (i >) (limit opts) = Result visited Nothing
        | Map.null fringe                = Result visited Nothing
        | (g, (c, _)) : _ <- goals       = Result visited' (Just (g, c))
        | otherwise                      = go (i + 1) visited' fringe'
      where
        goals    = filter (find opts . fst) $ Map.toList fringe
        visited' = visited `Map.union` fringe
        fringe'  = Map.fromList $ do
            (n, _) <- Map.toList fringe
            nb <- neighbours opts n
            guard . not $ nb `Map.member` visited'
            pure (nb, (i + 1, Just n))

reachable :: Result v -> [v]
reachable = Map.keys . back

distances :: Result v -> Map.Map v Int
distances = fmap fst . back

backtrack :: forall v. Ord v => v -> Result v -> [v]
backtrack end (Result b _) = go [end] end
  where
    go :: [v] -> v -> [v]
    go acc node = fromMaybe acc $ do
        (_, mbPrev) <- Map.lookup node b
        prev <- mbPrev
        pure $ go (prev : acc) prev
