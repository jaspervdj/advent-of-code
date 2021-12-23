{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
import qualified AdventOfCode.Grid   as G
import qualified AdventOfCode.V2     as V2
import qualified AdventOfCode.V2.Box as Box
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified System.IO           as IO

data Square = Open | Tree deriving (Show)

readSquare :: Char -> IO Square
readSquare = \case
    '#' -> pure Tree
    '.' -> pure Open
    c   -> fail $ "Unknown square: " <> show c

walk :: V2.V2 Int -> G.Grid Square -> Int
walk slope grid = go 0 (V2.V2 0 0)
  where
    box = fromMaybe (Box.Box V2.zero V2.zero) $ G.box grid
    get (V2.V2 x y) = Map.lookup (V2.V2 (x `mod` Box.width box) y) grid
    go !acc pos | V2.v2Y pos >= Box.height box = acc
    go !acc pos =
        let acc' = case get pos of Just Tree -> acc + 1; _ -> acc in
        go acc' $ pos V2..+. slope

main :: IO ()
main = do
    grid <- G.readGrid readSquare IO.stdin
    print $ walk (V2.V2 3 1) grid
    print $ product
        [ walk (V2.V2 1 1) grid
        , walk (V2.V2 3 1) grid
        , walk (V2.V2 5 1) grid
        , walk (V2.V2 7 1) grid
        , walk (V2.V2 1 2) grid
        ]
