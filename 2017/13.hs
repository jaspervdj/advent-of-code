{-# LANGUAGE BangPatterns #-}
import           Control.Monad       (forM_)
import           Data.Maybe          (listToMaybe, maybeToList)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import qualified System.IO           as IO
import           Text.Read           (readMaybe)

type Picos = Int
type Depth = Int
type Range = Int

type Scanners = V.Vector (Maybe Range)

scannerRangeAt :: Depth -> Scanners -> Maybe Int
scannerRangeAt d = (V.! d)

scannerPositionAt :: Picos -> Depth -> Scanners -> Maybe Int
scannerPositionAt !picos !depth scanners = do
    range <- scannerRangeAt depth scanners
    let !period = (range - 1) * 2
        !step   = picos `mod` period
        !pos    = if step < range then step else period - step
    return pos

readScanners :: IO.Handle -> IO Scanners
readScanners h = do
    input    <- lines <$> IO.hGetContents h
    scanners <- mapM parseScanner input
    let size = if null scanners then 0 else maximum (map fst scanners) + 1
    return $ V.create $ do
        vec <- VM.replicate size Nothing
        forM_ scanners $ \(d, r) -> VM.write vec d (Just r)
        return vec
  where
    parseScanner :: String -> IO (Depth, Range)
    parseScanner line =
        case words (map (\c -> if c == ':' then ' ' else c) line) of
            [d, r] | Just depth <- readMaybe d, Just range <- readMaybe r ->
                return (depth, range)
            _ -> fail $ "Cannot parse scanner: " ++ line

problem01 :: Scanners -> Int
problem01 scanners = sum
    [ t * range
    | t          <- [0 .. V.length scanners - 1]
    , range      <- maybeToList $ scannerRangeAt t scanners
    , scannerPos <- maybeToList $ scannerPositionAt t t scanners
    , scannerPos == 0
    ]

problem02 :: Scanners -> Maybe Int
problem02 scanners = listToMaybe
    [delay | delay <- [0 ..], not (caught delay)]
  where
    caught :: Picos -> Bool
    caught delay = or
        [ pos == 0
        | time <- [0 .. V.length scanners - 1]
        , pos  <- maybeToList $ scannerPositionAt (time + delay) time scanners
        ]

main :: IO ()
main = do
    scanners <- readScanners IO.stdin
    putStrLn $ "Severity: " ++ show (problem01 scanners)
    putStrLn $ "Delay: " ++ maybe "<unknown>" show (problem02 scanners)
