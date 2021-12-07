-- | Simple 2D grids backed by a vector.
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}
module AdventOfCode.Grid.Bounded
    ( G.Dir (..)
    , G.turnLeft
    , G.turnRight
    , G.turnAround

    , G.Pos
    , G.origin
    , G.move
    , G.neighbours
    , G.diagonal
    , G.manhattan

    , Grid (..)
    , generate
    , fromString
    , mapWithKey
    , lookup
    , index
    , toString
    , toList
    ) where

import qualified AdventOfCode.Grid as G
import           AdventOfCode.V2   (V2 (..))
import           Control.Monad     (when)
import           Data.Maybe        (fromMaybe)
import qualified Data.Vector       as V
import qualified Data.Vector.Extra as V (generate')
import           Prelude           hiding (lookup)

data Grid a = Grid
    { gridWidth  :: {-# UNPACK #-} !Int
    , gridHeight :: {-# UNPACK #-} !Int
    , gridData   :: {-# UNPACK #-} !(V.Vector a)
    } deriving (Eq, Foldable, Functor, Show, Traversable)

emptyGrid :: Grid a
emptyGrid = Grid 0 0 V.empty

generate :: Int -> Int -> (G.Pos -> a) -> Grid a
generate width height f = Grid
    { gridWidth  = width
    , gridHeight = height
    , gridData   = V.generate' (width * height) $ \idx ->
        let (y, x) = idx `divMod` width in f (V2 x y)
    }

fromString :: String -> Either String (Grid Char)
fromString string = case lines string of
    [] -> Right emptyGrid
    (x : xs) ->
        let row = V.fromList x
            width = V.length row in
        go width [row] xs
  where
    go width rows [] =
        Right $ Grid width (length rows) (V.concat $ reverse rows)
    go width rows (x : xs) = do
        let row = V.fromList x
        when (V.length row /= width) $ Left "row length mismatch"
        go width (row : rows) xs

mapWithKey :: (G.Pos -> a -> b) -> Grid a -> Grid b
mapWithKey f Grid {..} = generate gridWidth gridHeight $ \(V2 x y) ->
    f (V2 x y) $! V.unsafeIndex gridData (y * gridWidth + x)
{-# INLINABLE mapWithKey #-}

lookup :: G.Pos -> Grid a -> Maybe a
lookup (V2 x y) Grid {..}
    | x < 0 || x >= gridWidth || y < 0 || y >= gridHeight = Nothing
    | otherwise                                           = Just $
        V.unsafeIndex gridData (y * gridWidth + x)

index :: G.Pos -> Grid a -> a
index v g = fromMaybe
    (error $ "AdventOfCode.Grid.Bounded.index: out of bounds: " ++ show v)
    (lookup v g)

toString :: Grid Char -> String
toString g@Grid {..} = unlines $ do
    y <- [0 .. gridHeight - 1]
    [[fromMaybe ' ' $ lookup (V2 x y) g | x <- [0 .. gridWidth - 1]]]

toList :: Grid a -> [(G.Pos, a)]
toList g@Grid {..} = do
    y <- [0 .. gridHeight - 1]
    x <- [0 .. gridWidth - 1]
    pure (V2 x y, index (V2 x y) g)
