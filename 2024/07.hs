import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Data.List               (sortOn)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Map                as M
import           Data.Maybe              (isNothing, listToMaybe, mapMaybe)
import           Debug.Trace

data Equation = Equation Integer (NonEmpty Integer) deriving (Show)

parseEquations :: NP.Parser Char [Equation]
parseEquations = parseLines $ Equation <$>
    NP.decimal <* NP.char ':' <*> NP.many1 (NP.char ' ' *> NP.decimal)
  where
    parseLines p = NP.sepBy p NP.newline

evaluations
    :: [Integer -> Integer -> Integer] -> (Integer -> Bool)
    -> NonEmpty Integer -> [Integer]
evaluations operations stop = go
  where
    go (x :| _) | stop x = []
    go (x :| [])         = [x]
    go (x :| y : zs)     = operations >>= \f -> go (f x y :| zs)

numDigits :: Integer -> Integer
numDigits = go 0
  where
    go !acc !n
        | n < 10    = acc + 1
        | otherwise = go (acc + 1) (n `div` 10)

(|||) :: Integer -> Integer -> Integer
(|||) x y = x * 10 ^ numDigits y + y

main :: IO ()
main = pureMain $ \input -> do
    equations <- NP.runParser parseEquations input
    let solve ops = sum
            [ r
            | Equation r xs <- equations
            , any (== r) (evaluations ops (> r) xs)
            ]
    pure (pure (solve [(+), (*)]), pure (solve [(+), (*), (|||)]))
