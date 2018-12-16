import qualified AdventOfCode.Parsing as Parsing
import           Control.Monad        (foldM, forM, liftM2)
import           Data.Bits            ((.&.), (.|.))
import           Data.Function        (on)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Vector          as V
import qualified Data.Vector          as VU
import qualified System.IO            as IO

--------------------------------------------------------------------------------
-- Memory of our little machine

type Memory = VU.Vector Int

get :: Int -> Memory -> Either String Int
get i m
    | i < 0 || i >= VU.length m = Left ("get: out of bounds: " ++ show i)
    | otherwise                 = Right (m VU.! i)

set :: Int -> Int -> Memory -> Either String Memory
set i x m
    | i < 0 || i >= VU.length m = Left ("set: out of bounds: " ++ show i)
    | otherwise                 = Right (m VU.// [(i, x)])

--------------------------------------------------------------------------------
-- Operations

type Operation = Int -> Int -> Int -> Memory -> Either String Memory

addr, addi :: Operation
addr a b c m = liftM2 (+) (get a m) (get b m) >>= \x -> set c x m
addi a b c m = liftM2 (+) (get a m) (pure b) >>= \x -> set c x m

mulr, muli :: Operation
mulr a b c m = liftM2 (*) (get a m) (get b m) >>= \x -> set c x m
muli a b c m = liftM2 (*) (get a m) (pure b) >>= \x -> set c x m

banr, bani :: Operation
banr a b c m = liftM2 (.&.) (get a m) (get b m) >>= \x -> set c x m
bani a b c m = liftM2 (.&.) (get a m) (pure b) >>= \x -> set c x m

borr, bori :: Operation
borr a b c m = liftM2 (.|.) (get a m) (get b m) >>= \x -> set c x m
bori a b c m = liftM2 (.|.) (get a m) (pure b) >>= \x -> set c x m

setr, seti :: Operation
setr a _ c m = get a m >>= \x -> set c x m
seti a _ c m = set c a m

gtir, gtri, gtrr :: Operation
gtir a b c m = liftM2 (>) (pure a) (get b m) >>= \x -> set c (b2i x) m
gtri a b c m = liftM2 (>) (get a m) (pure b) >>= \x -> set c (b2i x) m
gtrr a b c m = liftM2 (>) (get a m) (get b m) >>= \x -> set c (b2i x) m

eqir, eqri, eqrr :: Operation
eqir a b c m = liftM2 (==) (pure a) (get b m) >>= \x -> set c (b2i x) m
eqri a b c m = liftM2 (==) (get a m) (pure b) >>= \x -> set c (b2i x) m
eqrr a b c m = liftM2 (==) (get a m) (get b m) >>= \x -> set c (b2i x) m

operations :: [(String, Operation)]
operations =
    [ ("addr", addr), ("addi", addi)
    , ("mulr", mulr), ("muli", muli)
    , ("banr", banr), ("bani", bani)
    , ("borr", borr), ("bori", bori)
    , ("setr", setr), ("seti", seti)
    , ("gtir", gtir), ("gtri", gtri), ("gtrr", gtrr)
    , ("eqir", eqir), ("eqri", eqri), ("eqrr", eqrr)
    ]

b2i :: Bool -> Int
b2i b = if b then 1 else 0

--------------------------------------------------------------------------------
-- Input and parsing

type Instruction = (Int, Int, Int, Int)

data Sample = Sample
    { sBefore      :: !Memory
    , sInstruction :: !Instruction
    , sAfter       :: !Memory
    } deriving (Show)

parseInput :: IO.Handle -> IO ([Sample], [Instruction])
parseInput h = do
    IO.hGetContents h >>= go . lines
  where
    go (lbefore : linstr : lafter : remainder)
        | "Before:" `L.isPrefixOf` lbefore
        , "After:" `L.isPrefixOf` lafter
        , [i0, i1, i2, i3] <- Parsing.ints linstr = do
            (samples, instrs) <- go (dropWhile null remainder)
            let sample = Sample
                    (VU.fromList (Parsing.ints lbefore))
                    (i0, i1, i2, i3)
                    (VU.fromList (Parsing.ints lafter))
            return (sample : samples, instrs)

    go (linstr : remainder)
        | [i0, i1, i2, i3] <- Parsing.ints linstr = do
            (samples, instrs) <- go remainder
            return (samples, (i0, i1, i2, i3) : instrs)

    go (l : _) = fail $ "Parsing failed at line: " ++ show l
    go [] = return ([], [])

--------------------------------------------------------------------------------
-- Finding matching operations

test :: Sample -> Operation -> Bool
test (Sample before (_, a, b, c) after) op = op a b c before == Right after

solve :: [Sample] -> Either String (V.Vector Operation)
solve samples = do
    -- Start with all posibilities for every opcode.
    let initialTable = M.fromList [(oc, operations) | oc <- [0 .. 15]]
        finalTable   = L.foldl' eliminate initialTable samples
        reducedTable = reduce ((==) `on` fst) finalTable

    -- Sanity check.
    decisions <- forM (M.toAscList reducedTable) $ \(i, ops) ->
        case ops of
            []          -> Left $ "No matching ops for code: " ++ show i
            (_ : _ : _) -> Left $ "Ambiguous ops for code: " ++ show i
            [(_, op)]   -> Right op

    return $ V.fromList decisions
  where
    eliminate table sample@(Sample _ (oc, _, _, _) _) =
        let ops0 = fromMaybe [] $ M.lookup oc table
            ops1 = filter (test sample . snd) ops0 in
        M.insert oc ops1 table

reduce :: Ord k => (v -> v -> Bool) -> M.Map k [v] -> M.Map k [v]
reduce equality bloated =
    let singles = [v | (_, [v]) <- M.toList bloated]
        elim v  = any (equality v) singles
        reduced = M.map (\vs -> case vs of
            [_] -> vs
            _   -> filter (not . elim) vs) bloated in
    if length (concat bloated) == length (concat reduced)
        then bloated
        else reduce equality reduced

--------------------------------------------------------------------------------
-- Running the program

runProgram :: V.Vector Operation -> [Instruction] -> Either String Memory
runProgram ops = foldM
    (\mem (o, a, b, c) -> (ops V.! o) a b c mem)
    (V.fromList [0, 0, 0, 0])

--------------------------------------------------------------------------------

main :: IO ()
main = do
    (samples, program) <- parseInput IO.stdin

    -- Part 1
    let numMatching = do
            sample <- samples
            return $ length [op | (_, op) <- operations, test sample op]
    print $ length $ filter (>= 3) numMatching

    -- Part 2
    solution <- either fail return (solve samples)
    mem      <- either fail return (runProgram solution program)
    print $ V.head mem
