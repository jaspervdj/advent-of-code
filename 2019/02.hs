module Main where

import           Data.Char     (isDigit)
import           Data.Maybe    (fromMaybe)
import qualified Data.Sequence as Seq
import qualified System.IO     as IO

data Machine = Machine Int (Seq.Seq Int) deriving (Show)

readMachine :: IO.Handle -> IO Machine
readMachine =
    fmap (Machine 0 . Seq.fromList . map read . words . map nonDigitToSpace) .
    IO.hGetContents
  where
    nonDigitToSpace x = if isDigit x then x else ' '

step :: Machine -> Either String Machine
step (Machine ip mem) = do
    op <- load ip
    case op of
        1  -> binop (+)
        2  -> binop (*)
        99 -> Left "Halted normally"
        _  -> Left $ "Unknown instruction: " ++ show op
  where
    load n = maybe (Left $ "Out of bounds: " ++ show n) Right (Seq.lookup n mem)

    store n x
        | n < 0 || n >= Seq.length mem = Left $ "Out of bounds: " ++ show n
        | otherwise                    = Right $ Seq.update n x mem

    binop f = do
        x   <- load (ip + 1) >>= load
        y   <- load (ip + 2) >>= load
        dst <- load (ip + 3)
        Machine (ip + 4) <$> store dst (f x y)

run :: Machine -> (Machine, String)
run machine = either ((,) machine) run $ step machine

setInput :: Int -> Int -> Machine -> Machine
setInput noun verb (Machine ip mem) =
    Machine ip $ Seq.update 1 noun $ Seq.update 2 verb mem

getOutput :: Machine -> Int
getOutput (Machine _ mem) = fromMaybe 0 $ Seq.lookup 0 mem

main :: IO ()
main = do
    machine <- readMachine IO.stdin
    print $ getOutput $ fst $ run $ setInput 12 2 machine
    print $ head
        [ noun * 100 + verb
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , getOutput (fst $ run $ setInput noun verb machine) == 19690720
        ]
