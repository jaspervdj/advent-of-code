{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.ST            (ST, runST)
import           Data.Bifunctor              (first)
import           Data.Char                   (chr, ord)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.IO                   as IO
import           Text.Read                   (readMaybe)

newtype Reg = Reg Int

instance Bounded Reg where
    minBound = mkReg 'a'
    maxBound = mkReg 'z'

instance Show Reg where
    show (Reg x) = [chr (x + ord 'a')]

instance Read Reg where
    readsPrec _ (x : str)
        | x >= 'a' && x <= 'z' = [(mkReg x, str)]
    readsPrec _ _              = []

mkReg :: Char -> Reg
mkReg c = Reg (ord c - ord 'a')

data Expression
    = LitE !Int
    | RegE !Reg
    deriving (Show)

instance Read Expression where
    readsPrec p str =
        map (first RegE) (readsPrec p str) ++
        map (first LitE) (readsPrec p str)

data Instruction
    = SetI !Reg !Expression
    | SubI !Reg !Expression
    | ModI !Reg !Expression
    | AddI !Reg !Expression
    | MulI !Reg !Expression
    | JnzI !Expression !Expression
    deriving (Show)

type Program = V.Vector Instruction

readProgram :: IO.Handle -> IO Program
readProgram h = do
    ls <- lines <$> IO.hGetContents h
    V.fromList <$> mapM parseInstruction ls
  where
    parseInstruction :: String -> IO Instruction
    parseInstruction line =
        maybe (fail $ "Can't parse instuction: " ++ line) return $
        case words line of
            ["set", x, y] -> SetI <$> readMaybe x <*> readMaybe y
            ["sub", x, y] -> SubI <$> readMaybe x <*> readMaybe y
            ["mul", x, y] -> MulI <$> readMaybe x <*> readMaybe y
            ["mod", x, y] -> ModI <$> readMaybe x <*> readMaybe y
            ["add", x, y] -> AddI <$> readMaybe x <*> readMaybe y
            ["jnz", x, y] -> JnzI <$> readMaybe x <*> readMaybe y
            _             -> Nothing

data Machine s = Machine
    { mProgram   :: !Program
    , mRegisters :: {-# UNPACK #-} !(VUM.MVector s Int)
    }

mulReg :: Reg
mulReg = Reg 62

new :: Program -> ST s (Machine s)
new mProgram = do
    mRegisters <- VUM.replicate 64 0
    return Machine {..}

set :: Machine s -> Reg -> Int -> ST s ()
set Machine {..} (Reg r) x = VUM.write mRegisters r x

get :: Machine s -> Reg -> ST s Int
get Machine {..} (Reg r) = VUM.read mRegisters r

run :: forall s. Machine s -> Int -> ST s ()
run machine@Machine {..} limit = go 0 0
  where
    go :: Int -> Int -> ST s ()
    go steps pc
        | steps > limit                     = return ()
        | pc < 0 || pc >= V.length mProgram = return ()
        | otherwise                         = case mProgram V.! pc of
            SetI r e -> do
                x <- eval e
                set machine r x
                go (steps + 1) (pc + 1)
            SubI r e -> do
                binop (-) r e
                go (steps + 1) (pc + 1)
            MulI r e -> do
                binop (*) r e
                binop (+) mulReg (LitE 1)
                go (steps + 1) (pc + 1)
            AddI r e -> do
                binop (+) r e
                go (steps + 1) (pc + 1)
            ModI r e -> do
                binop mod r e
                go (steps + 1) (pc + 1)
            JnzI e1 e2 -> do
                x      <- eval e1
                offset <- if x /= 0 then eval e2 else return 1
                go (steps + 1) (pc + offset)

    binop :: (Int -> Int -> Int) -> Reg -> Expression -> ST s ()
    binop f r e = do
        x <- get machine r
        y <- eval e
        set machine r (f x y)

    eval :: Expression -> ST s Int
    eval (LitE x) = return x
    eval (RegE r) = get machine r

problem01 :: Program -> Int
problem01 program = runST $ do
    machine <- new program
    run machine maxBound
    get machine mulReg

problem02 :: Program -> Int
problem02 program = runST $ do
    machine <- new program
    set machine (mkReg 'a') 1
    run machine 100
    b <- get machine (mkReg 'b')
    c <- get machine (mkReg 'c')
    return $ length $ filter id $ [not (isPrime x) | x <- [b, b + 17 .. c]]
  where
    isPrime :: Int -> Bool
    isPrime x
        | x == 1         = False
        | x == 2         = True
        | x `mod` 2 == 0 = False
        | otherwise      = go 3
      where
        go n
            | n * n > x      = True
            | x `mod` n == 0 = False
            | otherwise      = go (n + 2)

main :: IO ()
main = do
    program <- readProgram IO.stdin
    putStrLn $ "Num muls: " ++ show (problem01 program)
    putStrLn $ "h: " ++ show (problem02 program)
