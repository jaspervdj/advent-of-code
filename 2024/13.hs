import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..), (.+.))
import           Control.Applicative     (many)
import           Data.Maybe              (mapMaybe)
import           Data.Ratio              (Ratio, denominator, numerator)

data Machine = Machine
    { mButtonA :: V2 Int
    , mButtonB :: V2 Int
    , mPrize   :: V2 Int
    } deriving (Show)

parseMachine :: NP.Parser Char Machine
parseMachine = Machine
    <$> (V2 <$> (NP.string "Button A: X+" *> NP.decimal)
            <*> (NP.string ", Y+" *> NP.decimal <* NP.spaces))
    <*> (V2 <$> (NP.string "Button B: X+" *> NP.decimal)
            <*> (NP.string ", Y+" *> NP.decimal <* NP.spaces))
    <*> (V2 <$> (NP.string "Prize: X=" *> NP.decimal)
            <*> (NP.string ", Y=" *> NP.decimal <* NP.spaces))

solve :: Machine -> Maybe Integer
solve machine
    | denominator a /= 1 = Nothing
    | denominator b /= 1 = Nothing
    | otherwise          = Just $ numerator a * 3 + numerator b
  where
    V2 ax ay = fromIntegral <$> mButtonA machine
    V2 bx by = fromIntegral <$> mButtonB machine
    V2 px py = fromIntegral <$> mPrize   machine

    --     px = ax * a + bx * b
    --     py = ay * a + by * b
    --
    -- =>  px = ax * a + bx * b
    --     b = (py - ay * a) / by
    --
    -- =>  px = ax * a + bx * (py - ay * a) / by
    --
    -- =>  px = ax * a + (bx * py - bx * ay * a) / by
    --
    -- =>  px = ax * a + bx * py / by - bx * ay * a / by
    --
    -- =>  px - bx * py / by = ax * a - bx * ay * a / by
    --
    -- =>  px - bx * py / by = a * (ax - bx * ay / by)
    --
    -- =>  a = (px - bx * py / by) / (ax - bx * ay / by)
    a, b :: Ratio Integer
    a = (px - bx * py / by) / (ax - bx * ay / by)
    b = (py - ay * a) / by

main :: IO ()
main = pureMain $ \input -> do
    machines <- NP.runParser (many parseMachine) input
    let part1  = sum $ mapMaybe solve machines
        offset = 10000000000000
        part2  = sum $ mapMaybe solve
            [m {mPrize = mPrize m .+. V2 offset offset} | m <- machines]
    pure (pure part1, pure part2)
