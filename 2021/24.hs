{-# LANGUAGE DataKinds #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Z3         as Z3
import qualified Data.Map as M
import           Control.Applicative     (many, (<|>))

type Program = [Instruction]

data Register = W | X | Y | Z deriving (Bounded, Enum, Eq, Ord, Show)

data Operand = Literal Int | Register Register deriving (Show)

data Instruction
    = Inp Register
    | Add Register Operand
    | Mul Register Operand
    | Div Register Operand
    | Mod Register Operand
    | Eql Register Operand
    deriving (Show)

parseInstruction :: NP.Parser Char Instruction
parseInstruction =
    (Inp <$> (tok (NP.string "inp") *> tok register)) <|>
    (Add <$> (tok (NP.string "add") *> tok register) <*> tok operand) <|>
    (Mul <$> (tok (NP.string "mul") *> tok register) <*> tok operand) <|>
    (Div <$> (tok (NP.string "div") *> tok register) <*> tok operand) <|>
    (Mod <$> (tok (NP.string "mod") *> tok register) <*> tok operand) <|>
    (Eql <$> (tok (NP.string "eql") *> tok register) <*> tok operand)
  where
    tok p    = p <* NP.spaces
    operand  = (Literal <$> NP.signedDecimal) <|> (Register <$> register)
    register =
        (W <$ NP.char 'w') <|>
        (X <$ NP.char 'x') <|>
        (Y <$ NP.char 'y') <|>
        (Z <$ NP.char 'z')

parseProgram :: NP.Parser Char Program
parseProgram = many parseInstruction

toZ3 :: Bool -> Program -> Z3.Program
toZ3 part1 program = mconcat $
    [ Z3.declareConst i <>
        Z3.assert (Z3.bvsgt (Z3.var i) (Z3.int2bv (Z3.int 0))) <>
        Z3.assert (Z3.bvslt (Z3.var i) (Z3.int2bv (Z3.int 10)))
    | i <- inputs] <>
    [Z3.declareConst modelNumber] <>
    [Z3.assert $ Z3.var modelNumber Z3.== foldl1
        (\acc i -> Z3.bvadd i (Z3.bvmul acc (Z3.int2bv (Z3.int 10))))
        (map Z3.var inputs)] <>
    [go 0 0 initRegisters program] <>
    [(if part1 then Z3.maximize else Z3.minimize) (Z3.var modelNumber)] <>
    [Z3.checkSat] <>
    [Z3.eval (Z3.bv2int (Z3.var modelNumber))]
  where
    inputs :: [Z3.Var ('Z3.BitVecSort 64)]
    inputs = [Z3.mkVar $ "input" ++ show i | i <- [0 :: Int .. 13]]

    modelNumber :: Z3.Var ('Z3.BitVecSort 64)
    modelNumber = Z3.mkVar "modelnumber"

    initRegisters :: M.Map Register (Z3.Expr ('Z3.BitVecSort 64))
    initRegisters = M.fromList $ do
        reg <- [minBound .. maxBound]
        pure (reg, Z3.int2bv (Z3.int 0))

    operand
        :: M.Map Register (Z3.Expr ('Z3.BitVecSort 64))
        -> Operand
        -> Z3.Expr ('Z3.BitVecSort 64)
    operand regs (Register x) = regs M.! x
    operand regs (Literal x)  = Z3.int2bv (Z3.int x)

    go :: Int -> Int -> M.Map Register (Z3.Expr ('Z3.BitVecSort 64)) -> Program -> Z3.Program
    go ic fc regs instructions = case instructions of
        [] ->
            Z3.assert ((regs M.! Z) Z3.== Z3.int2bv (Z3.int 0)) <> []
        Inp reg : instrs ->
            go (ic + 1) fc (M.insert reg (Z3.var $ inputs !! ic) regs) instrs
        Add x y : instrs ->
            let fresh = Z3.mkVar $ "fresh" ++ show fc :: Z3.Var ('Z3.BitVecSort 64) in
            Z3.declareConst fresh <>
            Z3.assert (Z3.var fresh Z3.== Z3.bvadd (regs M.! x) (operand regs y)) <>
            go ic (fc + 1) (M.insert x (Z3.var fresh) regs) instrs
        Mul x y : instrs ->
            let fresh = Z3.mkVar $ "fresh" ++ show fc :: Z3.Var ('Z3.BitVecSort 64) in
            Z3.declareConst fresh <>
            Z3.assert (Z3.var fresh Z3.== Z3.bvmul (regs M.! x) (operand regs y)) <>
            go ic (fc + 1) (M.insert x (Z3.var fresh) regs) instrs
        Div x y : instrs ->
            let fresh = Z3.mkVar $ "fresh" ++ show fc :: Z3.Var ('Z3.BitVecSort 64) in
            Z3.declareConst fresh <>
            Z3.assert (Z3.not $ operand regs y Z3.== Z3.int2bv (Z3.int 0)) <>
            Z3.assert (Z3.var fresh Z3.== Z3.bvsdiv (regs M.! x) (operand regs y)) <>
            go ic (fc + 1) (M.insert x (Z3.var fresh) regs) instrs
        Mod x y : instrs ->
            let fresh = Z3.mkVar $ "fresh" ++ show fc :: Z3.Var ('Z3.BitVecSort 64) in
            Z3.declareConst fresh <>
            Z3.assert (Z3.not $ operand regs y Z3.== Z3.int2bv (Z3.int 0)) <>
            Z3.assert (Z3.var fresh Z3.== Z3.bvsmod (regs M.! x) (operand regs y)) <>
            go ic (fc + 1) (M.insert x (Z3.var fresh) regs) instrs
        Eql x y : instrs ->
            let fresh = Z3.mkVar $ "fresh" ++ show fc :: Z3.Var ('Z3.BitVecSort 64) in
            Z3.declareConst fresh <>
            Z3.assert (Z3.var fresh Z3.== Z3.ite ((regs M.! x) Z3.== (operand regs y))
                (Z3.int2bv (Z3.int 1)) (Z3.int2bv (Z3.int 0))) <>
            go ic (fc + 1) (M.insert x (Z3.var fresh) regs) instrs

main :: IO ()
main = ioMain $ \str -> do
    program <- either fail pure $ NP.runParser parseProgram str
    pure (Z3.run "2021-24" $ toZ3 True program, Z3.run "2021-24" $ toZ3 False program)
