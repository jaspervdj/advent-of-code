{-# LANGUAGE BangPatterns #-}
import           Data.Map  (Map)
import qualified Data.Map  as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)

data Operation = Inc | Dec deriving (Show)

parseOperation :: String -> Either String Operation
parseOperation "inc" = Right Inc
parseOperation "dec" = Right Dec
parseOperation op    = Left $ "Unknown operation: " ++ op

data Comparison
    = SmallerThan
    | SmallerThanOrEqual
    | Equal
    | NotEqual
    | LargerThan
    | LargerThanOrEqual
    deriving (Show)

parseComparison :: String -> Either String Comparison
parseComparison str = case str of
    "<"  -> Right SmallerThan
    "<=" -> Right SmallerThanOrEqual
    "==" -> Right Equal
    "!=" -> Right NotEqual
    ">"  -> Right LargerThan
    ">=" -> Right LargerThanOrEqual
    _    -> Left $ "Unknown comparison: " ++ str

type Register = String

parseRegister :: String -> Either String Register
parseRegister = Right

type Literal = Int

parseLiteral :: String -> Either String Literal
parseLiteral str = case readMaybe str of
    Just l  -> Right l
    Nothing -> Left $ "Bad literal: " ++ str

data Instruction
    = Instruction Register Operation Literal Register Comparison Literal
    deriving (Show)

parseInstruction :: String -> Either String Instruction
parseInstruction str = case words str of
    [a, inc, x, "if", b, comp, y] -> Instruction
        <$> parseRegister a
        <*> parseOperation inc
        <*> parseLiteral x
        <*> parseRegister b
        <*> parseComparison comp
        <*> parseLiteral y
    _ -> Left $ "Bad instruction: " ++ str

type Program = [Instruction]

parseProgram :: String -> Either String Program
parseProgram = mapM parseInstruction . lines

data Machine = Machine
    { mRegisters :: Map String Int
    , mHighest   :: Maybe Int
    }

zero :: Machine
zero = Machine {mRegisters = mempty, mHighest = Nothing}

runInstruction :: Instruction -> Machine -> Machine
runInstruction (Instruction a op x b comp y) machine =
    let !bv = fromMaybe 0 (M.lookup b (mRegisters machine)) in
    if evalComparision comp bv y then
        let !av0 = fromMaybe 0 (M.lookup a (mRegisters machine))
            !av1 = evalOperation op av0 x in
        machine
            { mRegisters = M.insert a av1 (mRegisters machine)
            , mHighest   = case mHighest machine of
                Nothing -> Just (max av0 av1)
                Just m  -> Just (max m av1)
            }
    else
        machine
  where
    evalComparision :: Comparison -> Literal -> Literal -> Bool
    evalComparision c = case c of
        SmallerThan        -> (<)
        SmallerThanOrEqual -> (<=)
        Equal              -> (==)
        NotEqual           -> (/=)
        LargerThan         -> (>)
        LargerThanOrEqual  -> (>=)

    evalOperation :: Operation -> Literal -> Literal -> Literal
    evalOperation Inc = (+)
    evalOperation Dec = (-)

runProgram :: Program -> Machine -> Machine
runProgram is m0 = foldl' (\m i -> runInstruction i m) m0 is

largestValue :: Machine -> Int
largestValue = maximum . map snd . M.toList . mRegisters

main :: IO ()
main = do
    input <- getContents
    case parseProgram input of
        Left err      -> fail err
        Right program -> do
            let machine = runProgram program zero
            putStrLn $ "Highest value at end: " ++ show (largestValue machine)
            putStrLn $
                "Highest value reached: " ++
                show (fromMaybe 0 $ mHighest machine)
