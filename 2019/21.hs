{-# LANGUAGE LambdaCase #-}
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import           Data.Char               (isSpace, ord)
import qualified System.IO               as IO

encodeSprintScript :: String -> [Int]
encodeSprintScript =
    map ord . unlines . filter (not . null) .
    map (removeWhitespace . dropComments) . lines
  where
    dropComments     = takeWhile (/= '#')
    removeWhitespace = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Manual labor
walker :: String
walker =
    "NOT A T  \n\
    \OR T J   \n\
    \NOT B T  \n\
    \OR T J   \n\
    \NOT C T  \n\
    \OR T J   \n\
    \AND D J  \n\
    \WALK"

-- More manual labor
runner :: String
runner =
    "# Part 1: A, B and C must be holes; D must be floor.   \n\
    \NOT A T                                                \n\
    \OR T J                                                 \n\
    \NOT B T                                                \n\
    \OR T J                                                 \n\
    \NOT C T                                                \n\
    \OR T J                                                 \n\
    \AND D J                                                \n\
    \                                                       \n\
    \# Part 2a: E is a hole, and K is floor so it's safe    \n\
    \# because we can immediately jump again.               \n\
    \NOT E T                                                \n\
    \AND H T                                                \n\
    \                                                       \n\
    \# Part 2b: Or E is floor so we can land and continue.  \n\
    \OR E T                                                 \n\
    \                                                       \n\
    \# Put together conclusions from part 1 (`J`) and part  \n\
    \# 2 (`T`).                                             \n\
    \AND T J                                                \n\
    \                                                       \n\
    \RUN"

main :: IO ()
main = do
    prog <- NP.hRunParser IO.stdin parseProgram
    print . last . evalMachine $ initMachine (encodeSprintScript walker) prog
    print . last . evalMachine $ initMachine (encodeSprintScript runner) prog
