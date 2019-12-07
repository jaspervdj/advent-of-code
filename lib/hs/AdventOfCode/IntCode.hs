-- | 2019's virtual machine, IntCode.
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module AdventOfCode.IntCode
    ( Program
    , parseProgram
    , makeProgram

    , Machine (..)
    , initMachine
    , stepMachine
    , runMachine
    , evalMachine
    ) where

import qualified AdventOfCode.NanoParser as NP
import           Data.Foldable           (toList)
import           Data.Maybe              (maybeToList)
import qualified Data.Sequence           as Seq

newtype Program = Program (Seq.Seq Int) deriving (Show)

parseProgram :: NP.Parser Char Program
parseProgram =
    Program . Seq.fromList <$> NP.sepBy1 (NP.signedDecimal) (NP.char ',')

makeProgram :: [Int] -> Program
makeProgram = Program . Seq.fromList

newtype Memory = Memory (Seq.Seq Int)

instance Show Memory where
    show (Memory s) = unwords $
        map (\(x, y) -> show x ++ ":" ++ show y) $ zip [0 :: Int ..] $ toList s

load :: Int -> Memory -> Either String Int
load n (Memory mem) = maybe
    (Left $ "load: Out of bounds: " ++ show n) Right (Seq.lookup n mem)

store :: Int -> Int -> Memory -> Either String Memory
store n x (Memory mem)
    | n < 0 || n >= Seq.length mem = Left $ "store: Out of bounds: " ++ show n
    | otherwise                    = Right $ Memory $ Seq.update n x mem

data Param = Position Int | Immediate Int deriving (Show)

data Instr p
    = Add         p p Int
    | Multiply    p p Int
    | Input           Int
    | Output      p
    | JumpIfTrue  p p
    | JumpIfFalse p p
    | LessThan    p p Int
    | Equals      p p Int
    | Halt
    deriving (Foldable, Functor, Show, Traversable)

instrSize :: Instr p -> Int
instrSize = \case
    Add _ _ _       -> 4
    Multiply _ _ _  -> 4
    Input _         -> 2
    Output _        -> 2
    JumpIfTrue _ _  -> 3
    JumpIfFalse _ _ -> 3
    LessThan _ _ _  -> 4
    Equals _ _ _    -> 4
    Halt            -> 1

loadInstr :: Int -> Memory -> Either String (Instr Param)
loadInstr ip mem = fmap parseInstr (load ip mem) >>= \case
    (1, pmodes)      -> parseBinop Add      pmodes
    (2, pmodes)      -> parseBinop Multiply pmodes
    (7, pmodes)      -> parseBinop LessThan pmodes
    (8, pmodes)      -> parseBinop Equals   pmodes
    (3, p1 : _)      -> Input <$> parsePosition p1 (ip + 1)
    (4, p1 : _)      -> Output <$> parseParam p1 (ip + 1)
    (5, p1 : p2 : _) -> JumpIfTrue
        <$> parseParam p1 (ip + 1)
        <*> parseParam p2 (ip + 2)
    (6, p1 : p2 : _) -> JumpIfFalse
        <$> parseParam p1 (ip + 1)
        <*> parseParam p2 (ip + 2)
    (99, _)          -> pure Halt
    (op, _)          -> Left $ "Unknown opcode: " ++ show op
  where
    parseInstr encoded =
        let (leading, op) = encoded `divMod` 100
            params l      = let (l', p) = l `divMod` 10 in p : params l' in
        (op, params leading)

    parseParam 0 n = Position  <$> load n mem
    parseParam 1 n = Immediate <$> load n mem
    parseParam m _ = Left $ "Invalid param mode: " ++ show m

    parsePosition m n = parseParam m n >>= \case
        Position  p -> pure p
        Immediate _ -> Left $ "Unexpected immediate param mode"

    parseBinop c (p1 : p2 : p3 : _) = c
        <$> parseParam p1 (ip + 1)
        <*> parseParam p2 (ip + 2)
        <*> parsePosition p3 (ip + 3)
    parseBinop _ _ = Left $ "Missing parameter modes"

data Machine = Machine
    { machineInputs :: [Int]
    , machineIp     :: Int
    , machineMemory :: Memory
    } deriving (Show)

initMachine :: [Int] -> Program -> Machine
initMachine inputs (Program mem) = Machine inputs 0 (Memory mem)

stepMachine :: Machine -> Either String (Machine, Maybe Int)
stepMachine (Machine inputs ip mem) = do
    instr <- loadInstr ip mem >>= traverse loadParam
    let ip' = instrSize instr + ip
    case instr of
        Add x y pos ->
            noOutput . Machine inputs ip' <$> store pos (x + y) mem
        Multiply x y pos ->
            noOutput . Machine inputs ip' <$> store pos (x * y) mem

        Output o -> do
            pure (Machine inputs ip' mem, Just o)

        Input pos -> case inputs of
            []            -> Left $ "No input available"
            (i : inputs') ->
                noOutput . Machine inputs' ip' <$> store pos i mem

        JumpIfTrue x dst ->
            pure $ noOutput $ Machine inputs (if x /= 0 then dst else ip') mem

        JumpIfFalse x dst ->
            pure $ noOutput $ Machine inputs (if x == 0 then dst else ip') mem

        LessThan x y pos ->
            noOutput . Machine inputs ip' <$>
            store pos (if x < y then 1 else 0) mem

        Equals x y pos ->
            noOutput . Machine inputs ip' <$>
            store pos (if x == y then 1 else 0) mem

        Halt -> Left $ "Machine halted normally"
  where
    loadParam (Immediate n) = pure n
    loadParam (Position p)  = load p mem

    noOutput m = (m, Nothing)

runMachine :: Machine -> ([Int], String)
runMachine machine = case stepMachine machine of
    Left err              -> ([], err)
    Right (machine', out) ->
        -- Set up recursion in a way that preserves laziness.
        let (output, err) = runMachine machine' in
        (maybeToList out ++ output, err)

evalMachine :: Machine -> [Int]
evalMachine = fst . runMachine
