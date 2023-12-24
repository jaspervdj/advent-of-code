{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import           AdventOfCode.V2.Box     (Box (..))
import qualified AdventOfCode.V2.Box     as Box
import           AdventOfCode.V3         (V3 (..))
import qualified AdventOfCode.Z3         as Z3
import           Data.Maybe              (maybeToList)
import qualified Data.Vector             as V

data Hailstone = Hailstone (V3 Int) (V3 Int) deriving (Show)

parseHailStones :: NP.Parser Char [Hailstone]
parseHailStones = NP.many1 $
    Hailstone <$> (parseV3 <* tok (NP.char '@')) <*> parseV3
  where
    tok p = p <* NP.spaces
    parseV3 = V3
        <$> (tok NP.signedDecimal <* tok (NP.char ','))
        <*> (tok NP.signedDecimal <* tok (NP.char ','))
        <*> tok NP.signedDecimal

crossXY :: Hailstone -> Hailstone -> Maybe (V2 Rational)
crossXY (Hailstone p0 v0) (Hailstone p1 v1)
    | (vx1 - vy1 / vy0 * vx0) == 0 = Nothing  -- Zero denominator
    | s < 0 || t < 0 = Nothing  -- Crossed in the past
    | otherwise  = Just (V2 x y)
  where
    V3 px0 py0 _ = toRational <$> p0
    V3 vx0 vy0 _ = toRational <$> v0
    V3 px1 py1 _ = toRational <$> p1
    V3 vx1 vy1 _ = toRational <$> v1

    -- Solve:
    --
    --     px0 + t * vx0 == px1 + s * vx1
    --     py0 + t * vy0 == py1 + s * vy1
    --
    -- First equation gives t in terms of s:
    --
    --     t == (px1 + s * vx1 - px0) / vx0
    --
    -- Substitute into second equation and solve for s:
    --
    --     py0 + (px1 + s * vx1 - px0) / vx0 * vy0 == py1 + s * vy1                ==>
    --     (px1 + s * vx1 - px0) == (py1 + s * vy1 - py0) / vy0 * vx0              ==>
    --     s * vx1 == (py1 + s * vy1 - py0) / vy0 * vx0 + px0 - px1                ==>
    --     s * vx1 - (s * vy1) / vy0 * vx0 == (py1 - py0) / vy0 * vx0 + px0 - px1  ==>
    --     s * (vx1 - vy1 / vy0 * vx0) == (py1 - py0) / vy0 * vx0 + px0 - px1      ==>
    --     s == ((py1 - py0) / vy0 * vx0 + px0 - px1) / (vx1 - vy1 / vy0 * vx0)
    --
    s = ((py1 - py0) / vy0 * vx0 + px0 - px1) / (vx1 - vy1 / vy0 * vx0)
    t = (px1 + s * vx1 - px0) / vx0

    -- Resulting x and y coordinates.
    x = toRational px1 + s * toRational vx1
    y = toRational py1 + s * toRational vy1

magicThrow :: [Hailstone] -> Z3.Program
magicThrow hailstones =
    Z3.declareConst mpx <> Z3.declareConst mpy <> Z3.declareConst mpz <>
    Z3.declareConst mvx <> Z3.declareConst mvy <> Z3.declareConst mvz <>
    mconcat [
        Z3.declareConst t <>
        Z3.assert (Z3.var mpx Z3.+ Z3.var t Z3.* Z3.var mvx Z3.==
            Z3.int px Z3.+ Z3.var t Z3.* Z3.int vx) <>
        Z3.assert (Z3.var mpy Z3.+ Z3.var t Z3.* Z3.var mvy Z3.==
            Z3.int py Z3.+ Z3.var t Z3.* Z3.int vy) <>
        Z3.assert (Z3.var mpz Z3.+ Z3.var t Z3.* Z3.var mvz Z3.==
            Z3.int pz Z3.+ Z3.var t Z3.* Z3.int vz)
    | (t, Hailstone (V3 px py pz) (V3 vx vy vz)) <- zip vars hailstones
    ] <>
    Z3.checkSat <>
    Z3.eval (Z3.toInt . Z3.add $ map Z3.var [mpx, mpy, mpz])
  where
    mpx, mpy, mpz, mvx, mvy, mvz :: Z3.Var 'Z3.RealSort
    mpx = "mpx"
    mpy = "mpy"
    mpz = "mpz"
    mvx = "mvx"
    mvy = "mvy"
    mvz = "mvz"

    vars = [Z3.mkVar $ "t" ++ show i | i <- [0 :: Int ..]]

main :: IO ()
main = ioMain $ \str -> do
    hailstones <- either fail pure $
        V.fromList <$> NP.runParser parseHailStones str
    let (lo, hi) = (200000000000000, 400000000000000)
        testArea = Box (V2 lo lo) (V2 hi hi)
        crossings = filter (`Box.inside` testArea) $ do
            i <- [0 .. V.length hailstones - 2]
            j <- [i + 1 .. V.length hailstones - 1]
            maybeToList $ crossXY (hailstones V.! i) (hailstones V.! j)
        part2 = magicThrow $ V.toList hailstones
    pure (pure (length crossings), Z3.run "2023-24" part2)
