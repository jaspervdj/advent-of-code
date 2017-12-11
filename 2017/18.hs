{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.ST            (ST, runST)
import           Data.Bifunctor              (first)
import           Data.Char                   (chr, ord)
import qualified Data.STRef                  as STRef
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable                 as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.IO                   as IO
import           Text.Read                   (readMaybe)

newtype Reg = Reg {unReg :: Int}

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
    = SndI !Expression
    | SetI !Reg !Expression
    | AddI !Reg !Expression
    | MulI !Reg !Expression
    | ModI !Reg !Expression
    | Rcv !Reg
    | Jgz !Expression !Expression
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
            ["snd", x]    -> SndI <$> readMaybe x
            ["set", x, y] -> SetI <$> readMaybe x <*> readMaybe y
            ["add", x, y] -> AddI <$> readMaybe x <*> readMaybe y
            ["mul", x, y] -> MulI <$> readMaybe x <*> readMaybe y
            ["mod", x, y] -> ModI <$> readMaybe x <*> readMaybe y
            ["rcv", x]    -> Rcv  <$> readMaybe x
            ["jgz", x, y] -> Jgz  <$> readMaybe x <*> readMaybe y
            _             -> Nothing

data Machine s = Machine
    { mProgram   :: !Program
    , mPc        :: !(STRef.STRef s Int)
    , mRegisters :: !(VUM.MVector s Int)
    }

new :: Program -> Int -> ST s (Machine s)
new mProgram pid = do
    mPc        <- STRef.newSTRef 0
    mRegisters <- VUM.replicate 64 0
    VUM.write mRegisters (unReg $ mkReg 'p') pid
    return Machine {..}

data Signal
    = SndS !Int !Int
    | RcvS !Reg !Int
    | StopS
    deriving (Show)

set :: Machine s -> Reg -> Int -> ST s ()
set Machine {..} (Reg r) x = VUM.write mRegisters r x

get :: Machine s -> Reg -> ST s Int
get Machine {..} (Reg r) = VUM.read mRegisters r

run :: forall s. Machine s -> Int -> ST s Signal
run machine@Machine {..} = go
  where
    go :: Int -> ST s Signal
    go pc
        | pc < 0 || pc >= V.length mProgram = return StopS
        | otherwise                         = case mProgram V.! pc of
            SndI e -> do
                x <- eval e
                return (SndS x (pc + 1))
            SetI r e -> do
                x <- eval e
                set machine r x
                go (pc + 1)
            AddI r e -> do
                binop (+) r e
                go (pc + 1)
            MulI r e -> do
                binop (*) r e
                go (pc + 1)
            ModI r e -> do
                binop mod r e
                go (pc + 1)
            Rcv r -> do
                return $ RcvS r (pc + 1)
            Jgz e1 e2 -> do
                x      <- eval e1
                offset <- if x > 0 then eval e2 else return 1
                go (pc + offset)

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
    machine <- new program 0
    go machine 0 0
  where
    go machine sound pc0 = do
        signal <- run machine pc0
        case signal of
            StopS      -> return 0
            SndS s pc1 -> go machine s pc1
            RcvS r pc1 -> do
                x <- get machine r
                if x /= 0 then return sound else go machine sound pc1

problem02 :: Program -> Int
problem02 program = runST $ do
    machines <- V.generateM   2 (\pid -> new program pid)
    states   <- VM.replicate  2 StopS
    queues   <- VM.replicate  2 []
    sends    <- VUM.replicate 3 (0 :: Int)

    let nextMachine n = (n + 1) `mod` (V.length machines)

        go n
            | n >= V.length machines = return ()
            | otherwise              = do
                let machine = machines V.! n
                state  <- VM.read states n
                queue  <- VM.read queues n
                case (state, queue) of
                    (StopS, _)            -> do
                        state1 <- run machine 0
                        VM.write states n state1
                        go 0
                    (RcvS _ _, [])        -> go (n + 1)
                    (RcvS r pc, (x : xs)) -> do
                        VM.write queues n xs
                        set machine r x
                        state1 <- run machine pc
                        VM.write states n state1
                        go 0
                    (SndS s pc, _) -> do
                        xs <- VM.read queues (nextMachine n)
                        VM.write queues (nextMachine n) (xs ++ [s])
                        state1 <- run machine pc
                        VM.write states n state1
                        sends0 <- VUM.read sends n
                        VUM.write sends n (sends0 + 1)
                        go 0

    go 0
    VUM.read sends 1

main :: IO ()
main = do
    program <- readProgram IO.stdin
    putStrLn $ "First recovered: " ++ show (problem01 program)
    putStrLn $ "Num sends (pid 1): " ++ show (problem02 program)
