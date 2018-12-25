{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified AdventOfCode.Parsing as Parsing
import qualified System.IO            as IO

data V4 = V4
    { vX :: {-# UNPACK #-} !Int
    , vY :: {-# UNPACK #-} !Int
    , vZ :: {-# UNPACK #-} !Int
    , vW :: {-# UNPACK #-} !Int
    } deriving (Show)

manhattan :: V4 -> V4 -> Int
manhattan l r =
    abs (vX l - vX r) +
    abs (vY l - vY r) +
    abs (vZ l - vZ r) +
    abs (vW l - vW r)

newtype Constellation = Constellation [V4] deriving (Semigroup, Show)

close :: Constellation -> Constellation -> Bool
close (Constellation ls) (Constellation rs) = or
    [manhattan l r <= 3 | l <- ls, r <- rs]

readPoints :: IO.Handle -> IO [V4]
readPoints h =
    IO.hGetContents h >>= mapM parseLine . lines
  where
    parseLine l = case Parsing.ints l of
        [x, y, z, w] -> return $ V4 x y z w
        _            -> fail $ "Could not parse line: " ++ l

cluster :: [V4] -> [Constellation]
cluster = \points -> run [] (map (Constellation . return) points)
  where
    -- We work with a pair of constellations.  Constellations in the right part
    -- are ones that have been updated in the last iteration.  These need to be
    -- checked against every other constellation.  Constellations in the left
    -- part, however, do not need to be checked against each other, since they
    -- have not changed.
    run old []  = old
    run old new = let (old', new') = step [] old new in run old' new'

    step acc old [] = (old, acc)
    step acc old (x : ys) =
        case break (close x) ys of
            (yl, (y : yr)) -> step ((x <> y) : acc) old (yl ++ yr)
            (_, []) -> case break (close x) old of
                (oldl, (o : oldr)) -> step ((x <> o) : acc) (oldl ++ oldr) ys
                (_, [])            -> step acc (x : old) ys

main :: IO ()
main = do
    points <- readPoints IO.stdin
    print $ length $ cluster points
