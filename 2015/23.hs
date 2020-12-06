{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Main where

import qualified AdventOfCode.NanoParser    as P
import           Control.Applicative        ((<|>))
import           Control.Monad              (when)
import           Control.Monad.State.Strict (State, execState, gets, modify)
import           Data.Functor               (($>))
import qualified Data.Vector                as V
import qualified System.IO                  as IO

data Reg = A | B deriving (Show)

data Instr
    = Hlf !Reg
    | Tpl !Reg
    | Inc !Reg
    | Jmp !Int
    | Jie !Reg !Int
    | Jio !Reg !Int
    deriving (Show)

type Program = V.Vector Instr

parseInstr :: P.Parser Char Instr
parseInstr =
    instr Hlf "hlf" <*> reg <|>
    instr Tpl "tpl" <*> reg <|>
    instr Inc "inc" <*> reg <|>
    instr Jmp "jmp" <*> offset <|>
    instr Jie "jie" <*> reg <* P.char ',' <* P.spaces <*> offset <|>
    instr Jio "jio" <*> reg <* P.char ',' <* P.spaces <*> offset
  where
    instr f s = P.string s *> P.spaces $> f
    reg       = (A <$ P.char 'a') <|> (B <$ P.char 'b')
    offset    = (P.char '+' *> P.decimal) <|> P.signedDecimal

parseProgram :: P.Parser Char Program
parseProgram = V.fromList <$> P.sepBy parseInstr (P.char '\n')

data Computer = Computer
    { cPc :: !Int
    , cA  :: !Int
    , cB  :: !Int
    } deriving (Show)

runInstr :: Instr -> State Computer ()
runInstr = \case
    Hlf r -> modifyReg r (`div` 2) >> incPc
    Tpl r -> modifyReg r (* 3) >> incPc
    Inc r -> modifyReg r succ >> incPc
    Jmp o -> addPc o
    Jie r o -> do
        x <- getReg r
        if even x then addPc o else incPc
    Jio r o -> do
        x <- getReg r
        if x == 1 then addPc o else incPc
  where
    addPc o = modify $ \c -> c {cPc = cPc c + o}
    incPc   = addPc 1

    getReg    A = gets cA
    getReg    B = gets cB
    modifyReg A f = modify (\c -> c {cA = f (cA c)})
    modifyReg B f = modify (\c -> c {cB = f (cB c)})

runProgram :: Program -> State Computer ()
runProgram program = do
    pc <- gets cPc
    when (pc >= 0 && pc < V.length program) $ do
        runInstr $ program V.! pc
        runProgram program

main :: IO ()
main = do
    program <- P.hRunParser IO.stdin parseProgram
    print . cB . execState (runProgram program) $ Computer 0 0 0
    print . cB . execState (runProgram program) $ Computer 0 1 0
