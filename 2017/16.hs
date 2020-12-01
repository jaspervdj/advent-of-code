{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Char           (chr, ord)
import           Data.List           (foldl')
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as VU
import qualified System.IO           as IO
import           Text.Read           (readMaybe)

newtype Dancer = Dancer Int

instance Read Dancer where
    readsPrec _ (c : x)
        | c >= 'a' && c <= 'z' = [(Dancer (ord c - ord 'a'), x)]
    readsPrec _ _              = []

instance Show Dancer where
    show (Dancer o) = [chr (ord 'a' + o)]

type Index = Int

data Move
    = Spin     !Index
    | Exchange !Index  !Index
    | Partner  !Dancer !Dancer
    deriving (Show)

readMoves :: IO.Handle -> IO [Move]
readMoves h = do
    items <- split <$> IO.hGetContents h
    mapM parseMove items
  where
    split :: String -> [String]
    split str0 = case break (== ',') str0 of
        (x, ',' : str1) -> x : split str1
        (x, _)          -> [x]

    parseMove :: String -> IO Move
    parseMove ('s' : str) = Spin <$> parseRead str
    parseMove ('x' : str) = uncurry Exchange <$> parsePair str
    parseMove ('p' : str) = uncurry Partner <$> parsePair str
    parseMove str         = fail $ "Unknown move: " ++ str

    parsePair :: Read a => String -> IO (a, a)
    parsePair str = case break (== '/') str of
        (x, '/' : y) -> (,) <$> parseRead x <*> parseRead y
        _            -> fail "Expected '/'"

    parseRead :: Read a => String -> IO a
    parseRead str =
        maybe (fail $ "Could not read " ++ str) return (readMaybe str)

newtype Dancers = Dancers (VU.Vector Int) deriving (Eq)

instance Show Dancers where
    show (Dancers v) = concatMap (show . Dancer) (VU.toList v)

zero :: Int -> Dancers
zero len = Dancers (VU.enumFromN 0 len)

move :: Move -> Dancers -> Dancers
move (Spin n) (Dancers v) =
    let !pre  = VU.slice 0 (VU.length v - n) v
        !post = VU.slice (VU.length v - n) n v in
    Dancers (post <> pre)

move (Exchange ix iy) (Dancers v) =
    let !x = v VU.! ix
        !y = v VU.! iy in
    Dancers (v VU.// [(ix, y), (iy, x)])

move (Partner (Dancer x) (Dancer y)) (Dancers v) =
    fromMaybe (Dancers v) $ do
        !ix <- VU.findIndex (== x) v
        !iy <- VU.findIndex (== y) v
        return $! Dancers (v VU.// [(ix, y), (iy, x)])

dance :: [Move] -> Dancers -> Dancers
dance moves dancers = foldl' (\acc m -> move m acc) dancers moves

period :: [Move] -> Dancers -> Int
period moves d0 = go 0 d0
  where
    go !n d =
        let !d' = dance moves d in
        if d' == d0 then (n + 1) else go (n + 1) d'

main :: IO ()
main = do
    moves <- readMoves IO.stdin
    let one = dance moves (zero 16)
    putStrLn $ "After one dance: " ++ show one

    let p      = period moves (zero 16)
        amount = 1000000000
        actual = amount `mod` p
        final  = foldl' (\d _ -> dance moves d) (zero 16) [1 .. actual]

    putStrLn $ "Final: " ++ show final
