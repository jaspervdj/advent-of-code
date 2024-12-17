import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Monad           (guard)
import           Data.Bits               (xor)
import           Data.List               (intercalate, isSuffixOf)
import           Data.Maybe              (maybeToList)
import qualified Data.Vector             as V

data Spec = Spec
    { sRegisterA :: Int
    , sRegisterB :: Int
    , sRegisterC :: Int
    , sProgram   :: [Int]
    } deriving (Show)

parseSpec :: NP.Parser Char Spec
parseSpec = Spec
    <$> (NP.string "Register A: " *> NP.decimal <* NP.spaces)
    <*> (NP.string "Register B: " *> NP.decimal <* NP.spaces)
    <*> (NP.string "Register C: " *> NP.decimal <* NP.spaces)
    <*> (NP.string "Program: " *> NP.sepBy NP.decimal (NP.char ','))

data Register = A | B | C deriving (Eq, Ord, Show)
data Registers = Registers !Int !Int !Int deriving (Show)

getRegister :: Register -> Registers -> Int
getRegister A (Registers a _ _) = a
getRegister B (Registers _ b _) = b
getRegister C (Registers _ _ c) = c

setRegister :: Register -> Int -> Registers -> Registers
setRegister A a (Registers _ b c) = Registers a b c
setRegister B b (Registers a _ c) = Registers a b c
setRegister C c (Registers a b _) = Registers a b c

data Computer = Computer
    { cRegisters          :: !Registers
    , cInstructionPointer :: !Int
    , cProgram            :: !(V.Vector Int)
    } deriving (Show)

specToComputer :: Spec -> Computer
specToComputer s = Computer
    { cRegisters = Registers (sRegisterA s) (sRegisterB s) (sRegisterC s)
    , cInstructionPointer = 0
    , cProgram = V.fromList (sProgram s)
    }

data Instruction = Adv | Bxl | Bst | Jnz | Bxz | Out | Bdv | Cdv
    deriving (Show)

parseInstruction :: Int -> Maybe Instruction
parseInstruction opcode = case opcode of
    0 -> pure Adv
    1 -> pure Bxl
    2 -> pure Bst
    3 -> pure Jnz
    4 -> pure Bxz
    5 -> pure Out
    6 -> pure Bdv
    7 -> pure Cdv
    _ -> Nothing

stepComputer :: Computer -> Maybe (Maybe Int, Computer)
stepComputer c = do
    opcode <- readProgram $ cInstructionPointer c
    instruction <- parseInstruction opcode
    case instruction of
        Adv -> xdv A
        Bxl -> do
            let x = readRegister B
            y <- literalOperand
            pure (Nothing, nextInstruction $ writeRegister B (xor x y) c)
        Bst -> do
            x <- comboOperand
            pure (Nothing, nextInstruction $ writeRegister B (x `mod` 8) c)
        Jnz | readRegister A == 0 ->
            pure (Nothing, nextInstruction c)
        Jnz -> do
            x <- literalOperand
            pure (Nothing, c {cInstructionPointer = x})
        Bxz -> do
            _ <- comboOperand
            let x = readRegister B
                y = readRegister C
            pure (Nothing, nextInstruction $ writeRegister B (xor x y) c)
        Out -> do
            x <- comboOperand
            pure (Just (x `mod` 8), nextInstruction c)
        Bdv -> xdv B
        Cdv -> xdv C
  where
    xdv r = do
        let x = readRegister A
        y <- comboOperand
        pure (Nothing, nextInstruction $ writeRegister r (x `div` (2 ^ y)) c)

    readProgram i
        | i < 0 || i >= V.length (cProgram c) = Nothing
        | otherwise                           = Just $ cProgram c V.! i

    nextInstruction comp = comp
        { cInstructionPointer = cInstructionPointer comp + 2
        }

    readRegister r = getRegister r (cRegisters c)
    writeRegister r !x comp = comp
        { cRegisters = setRegister r x (cRegisters comp)
        }

    literalOperand = readProgram (cInstructionPointer c + 1)
    comboOperand   = do
        code <- literalOperand
        case code of
            0 -> pure 0
            1 -> pure 1
            2 -> pure 2
            3 -> pure 3
            4 -> pure $ readRegister A
            5 -> pure $ readRegister B
            6 -> pure $ readRegister C
            7 -> Nothing -- reserved
            _ -> Nothing

evalComputer :: Computer -> [Int]
evalComputer c = case stepComputer c of
    Nothing        -> []
    Just (out, c') -> maybeToList out ++ evalComputer c'

searchQuine :: Computer -> [Int]
searchQuine comp = go 0
  where
    go a = (a <$ guard (run a == program)) ++ do
        a' <- [a * 8 + b | b <- [0 .. 8]]
        guard $ run a' `isSuffixOf` program
        go a'

    program = V.toList (cProgram comp)
    run a   = evalComputer comp {cRegisters = setRegister A a (cRegisters comp)}

main :: IO ()
main = pureMain $ \input -> do
    spec <- NP.runParser parseSpec input
    let computer = specToComputer spec
        part1 = intercalate "," $ map show $ evalComputer computer
        part2 = case searchQuine computer of
            []    -> Left "no solution"
            a : _ -> pure a
    pure (pure part1, part2)
