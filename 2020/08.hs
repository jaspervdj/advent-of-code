{-# LANGUAGE RecordWildCards #-}
module Main where

import           AdventOfCode.Dijkstra   (bfs, bfsGoal)
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))
import           Data.List               (find)
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

-- Current instruction number and whether or not we've already switched an
-- instruction.  Doing a simple forward search on this can show us which
-- instruction to switch.
data Vertex = Vertex !Int !Bool deriving (Eq, Ord, Show)

controlFlow :: Program -> Maybe Int
controlFlow program = do
    (_, path) <- bfsGoal $ bfs neighbours goal (Vertex 0 False)
    Vertex i _ <- find (\(Vertex _ s) -> not s) path
    pure i
  where
    valid, goal :: Vertex -> Bool
    valid (Vertex i _) = i >= 0 && i <= V.length program
    goal  (Vertex i _) = i == V.length program

    neighbours :: Vertex -> [Vertex]
    neighbours (Vertex i switched) = filter valid $ case program V.! i of
        Acc _            -> [Vertex (i + 1) switched]
        Nop _ | switched -> [Vertex (i + 1) switched]
        Nop n            -> [Vertex (i + 1) switched, Vertex (i + n) True]
        Jmp n | switched -> [Vertex (i + n) switched]
        Jmp n            -> [Vertex (i + n) switched, Vertex (i + 1) True]

main :: IO ()
main = do
    program <- P.hRunParser IO.stdin parseProgram
    case runProgram program of
        Looping acc -> print acc
        _           -> fail "no solution"

    switch <- case controlFlow program of
        Nothing -> fail "no switch?"
        Just s  -> pure s

    let program' = case program V.! switch of
            Jmp n -> program V.// [(switch, Nop n)]
            Nop n -> program V.// [(switch, Jmp n)]
            Acc _ -> program

    case runProgram program' of
        Terminate acc -> print acc
        _             -> fail "not terminating"
