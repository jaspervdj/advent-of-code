-- | 2019's virtual machine, IntCode.
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
module AdventOfCode.IntCode
    ( Interrupt (..)

    , Program
    , parseProgram
    , makeProgram

    , Machine (..)
    , initMachine
    , stepMachine
    , runMachine
    , evalMachine

    , runAsciiMachine

    , test
    ) where

import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.NanoTest   as NT
import           Data.Char               (chr, ord)
import qualified Data.IntMap             as IM
import           Data.Maybe              (fromMaybe, maybeToList)
import           Data.Semigroup          (Semigroup)
import qualified System.IO               as IO

data Interrupt
    = HaltSuccess
    | OutOfBounds String Int
    | InsufficientInput
    | UnknownOpcode Int
    | IllegalParams String
    deriving (Show)

newtype Program = Program (IM.IntMap Int) deriving (Semigroup, Show)

parseProgram :: NP.Parser Char Program
parseProgram = makeProgram <$> NP.sepBy1 (NP.signedDecimal) (NP.char ',')

makeProgram :: [Int] -> Program
makeProgram = Program . IM.fromList . zip [0 ..]

newtype Memory = Memory (IM.IntMap Int)

instance Show Memory where
    show (Memory s) = unwords $
        map (\(x, y) -> show x ++ ":" ++ show y) $ IM.toList s

load :: Int -> Memory -> Either Interrupt Int
load n (Memory mem)
    | n < 0     = Left $ OutOfBounds "load" n
    | otherwise = Right . fromMaybe 0 $ IM.lookup n mem

store :: Int -> Int -> Memory -> Either Interrupt Memory
store n x (Memory mem)
    | n < 0     = Left $ OutOfBounds "store" n
    | otherwise = Right $ Memory $ IM.insert n x mem

-- | Points to a memory location.
data Pointer = Absolute !Int | Relative !Int deriving (Show)

-- | Can be read but not always written to.
data Param = Position !Pointer | Immediate !Int deriving (Show)

data Instr p
    = Add                p p Pointer
    | Multiply           p p Pointer
    | Input                  Pointer
    | Output             p
    | JumpIfTrue         p p
    | JumpIfFalse        p p
    | LessThan           p p Pointer
    | Equals             p p Pointer
    | RelativeBaseOffset p
    | Halt
    deriving (Foldable, Functor, Show, Traversable)

instrSize :: Instr p -> Int
instrSize = \case
    Add _ _ _            -> 4
    Multiply _ _ _       -> 4
    Input _              -> 2
    Output _             -> 2
    JumpIfTrue _ _       -> 3
    JumpIfFalse _ _      -> 3
    LessThan _ _ _       -> 4
    Equals _ _ _         -> 4
    RelativeBaseOffset _ -> 2
    Halt                 -> 1

loadInstr :: Int -> Memory -> Either Interrupt (Instr Param)
loadInstr ip mem = fmap parseInstr (load ip mem) >>= \case
    (1, pmodes)      -> parseBinop Add      pmodes
    (2, pmodes)      -> parseBinop Multiply pmodes
    (7, pmodes)      -> parseBinop LessThan pmodes
    (8, pmodes)      -> parseBinop Equals   pmodes
    (3, p1 : _)      -> Input <$> parsePointer p1 (ip + 1)
    (4, p1 : _)      -> Output <$> parseParam p1 (ip + 1)
    (5, p1 : p2 : _) -> JumpIfTrue
        <$> parseParam p1 (ip + 1)
        <*> parseParam p2 (ip + 2)
    (6, p1 : p2 : _) -> JumpIfFalse
        <$> parseParam p1 (ip + 1)
        <*> parseParam p2 (ip + 2)
    (9, p1 : _)      -> RelativeBaseOffset <$> parseParam p1 (ip + 1)
    (99, _)          -> pure Halt
    (op, _)          -> Left $ UnknownOpcode op
  where
    parseInstr encoded =
        let (leading, op) = encoded `divMod` 100
            params l      = let (l', p) = l `divMod` 10 in p : params l' in
        (op, params leading)

    parseParam 0 n = Position . Absolute <$> load n mem
    parseParam 1 n = Immediate           <$> load n mem
    parseParam 2 n = Position . Relative <$> load n mem
    parseParam m _ = Left $ IllegalParams $ "Invalid param mode: " ++ show m

    parsePointer m n = parseParam m n >>= \case
        Position  p -> pure p
        Immediate _ -> Left $ IllegalParams $ "Unexpected immediate param mode"

    parseBinop c (p1 : p2 : p3 : _) = c
        <$> parseParam   p1 (ip + 1)
        <*> parseParam   p2 (ip + 2)
        <*> parsePointer p3 (ip + 3)
    parseBinop _ _ = Left $ IllegalParams $ "Missing parameter modes for binop"

data Machine = Machine
    { mInputs  :: [Int]
    , mRelBase :: Int
    , mIp      :: Int
    , mMem     :: Memory
    } deriving (Show)

initMachine :: [Int] -> Program -> Machine
initMachine inputs (Program mem) = Machine inputs 0 0 (Memory mem)

stepMachine :: Machine -> Either Interrupt (Machine, Maybe Int)
stepMachine machine@Machine {..} = do
    let resolvePointer (Absolute p) = p
        resolvePointer (Relative p) = mRelBase + p

        loadParam (Immediate n) = pure n
        loadParam (Position p)  = load (resolvePointer p) mMem

    instr <- loadInstr mIp mMem >>= traverse loadParam

    let machine'      = machine {mIp = mIp + instrSize instr}
        binop f x y p = do
            mem <- store (resolvePointer p) (f x y) mMem
            pure (machine' {mMem = mem}, Nothing)

    case instr of
        Add      x y p -> binop (+)                               x y p
        Multiply x y p -> binop (*)                               x y p
        LessThan x y p -> binop (\l r -> if l < r  then 1 else 0) x y p
        Equals   x y p -> binop (\l r -> if l == r then 1 else 0) x y p

        Output o -> pure (machine', Just o)
        Input  p -> case mInputs of
            []     -> Left InsufficientInput
            i : is -> do
                m <- store (resolvePointer p) i mMem
                pure (machine' {mMem = m, mInputs = is}, Nothing)

        JumpIfTrue x dst -> pure
            (if x /= 0 then machine {mIp = dst} else machine', Nothing)

        JumpIfFalse x dst -> pure
            (if x == 0 then machine {mIp = dst} else machine', Nothing)

        RelativeBaseOffset o -> pure
            (machine' {mRelBase = mRelBase + o}, Nothing)

        Halt -> Left HaltSuccess

runMachine :: Machine -> ([Int], Interrupt, Machine)
runMachine machine = case stepMachine machine of
    Left err              -> ([], err, machine)
    Right (machine', out) ->
        -- Set up recursion in a way that preserves laziness.
        let (output, err, machine'') = runMachine machine' in
        (maybeToList out ++ output, err, machine'')

evalMachine :: Machine -> [Int]
evalMachine = (\(o, _, _) -> o) . runMachine

-- | Run a machine that reads and prints ASCII interactively.  Mostly useful for
-- debugging.
runAsciiMachine :: (IO.Handle, IO.Handle) -> Program -> IO ()
runAsciiMachine (inh, outh) prog = do
    IO.hSetBuffering inh IO.NoBuffering
    input <- map ord <$> IO.hGetContents inh
    let (outputs, interrupt, _) = runMachine $ initMachine input prog
    IO.hPutStrLn outh $ concatMap renderAsciiOrNumber outputs
    case interrupt of
        HaltSuccess -> pure ()
        _           -> IO.hPutStrLn outh $ show interrupt

renderAsciiOrNumber :: Int -> String
renderAsciiOrNumber i = if i <= 0xff then [chr i] else show i

test :: IO ()
test = do
    let quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    evalMachine (initMachine [] (makeProgram quine)) NT.@?= quine

    evalMachine (initMachine [] $
        makeProgram [1102,34915192,34915192,7,4,7,99,0]) NT.@?=
        [1219070632396864]
