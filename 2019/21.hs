{-# LANGUAGE LambdaCase #-}
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import           Data.Char               (chr, ord)
import           System.Environment      (getArgs)
import qualified System.IO               as IO

renderAsciiOrNumber :: Int -> String
renderAsciiOrNumber i = if i <= 0xff then [chr i] else show i

interactively :: Program -> IO ()
interactively prog = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    input <- map ord <$> IO.hGetContents IO.stdin
    let (outputs, interrupt, _) = runMachine $ initMachine input prog
    putStrLn $ concatMap renderAsciiOrNumber outputs
    case interrupt of
        HaltSuccess -> pure ()
        _           -> putStrLn $ show interrupt

-- Manual labor
walkScript :: String
walkScript = concat $ map (<> "\n")
    [ "NOT A T"
    , "OR T J"
    , "NOT B T"
    , "OR T J"
    , "NOT C T"
    , "OR T J"
    , "AND D J"
    , "WALK"
    ]

main :: IO ()
main = do
    [file] <- getArgs
    prog   <- IO.withFile file IO.ReadMode $ \h -> NP.hRunParser h parseProgram
    print . last . evalMachine $ initMachine (map ord walkScript) prog
    interactively prog
