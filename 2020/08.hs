{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))
import qualified Data.Set                as Set
import qualified Data.Vector             as V
import qualified System.IO               as IO

type Program = V.Vector Instr

data Instr
    = Acc !Int
    | Jmp !Int
    | Nop !Int
    deriving (Show)

parseProgram :: P.Parser Char Program
parseProgram = V.fromList <$> P.sepBy parseInstr (P.char '\n')

parseInstr :: P.Parser Char Instr
parseInstr =
    (Acc <$> instr "acc") <|>
    (Jmp <$> instr "jmp") <|>
    (Nop <$> instr "nop")
  where
    instr str = P.string str *> P.spaces *> arg
    arg = (P.char '+' *> P.decimal) <|> P.signedDecimal

data Console = Console
    { cPc  :: !Int
    , cAcc :: !Int
    } deriving (Show)

emptyConsole :: Console
emptyConsole = Console 0 0

runInstr :: Instr -> Console -> Console
runInstr instr c@Console {..} = case instr of
    Acc a -> c {cAcc = cAcc + a, cPc = cPc + 1}
    Jmp o -> c {cPc = cPc + o}
    Nop _ -> c {cPc = cPc + 1}

data Exit = Looping Int | Crash | Terminate Int deriving (Show)

runProgram :: Program -> Exit
runProgram program = go Set.empty emptyConsole
  where
    go visited c
        | cPc c `Set.member` visited            = Looping $ cAcc c
        | cPc c == V.length program             = Terminate $ cAcc c
        | cPc c < 0 || cPc c > V.length program = Crash
        | otherwise                             =
            let instr = program V.! cPc c in
            go (Set.insert (cPc c) visited) (runInstr instr c)

main :: IO ()
main = do
    program <- P.hRunParser IO.stdin parseProgram
    case runProgram program of
        Looping acc -> print acc
        _           -> fail "no solution"

    let terminating = do
            idx <- [0 .. V.length program - 1]
            instr <- case program V.! idx of
                Jmp n -> [Nop n]
                Nop n -> [Jmp n]
                Acc _ -> []
            case runProgram $ program V.// [(idx, instr)] of
                Terminate acc -> [acc]
                _             -> []

    print $ head terminating
