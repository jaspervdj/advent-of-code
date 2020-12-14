{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))
import           Data.Bits               (shiftL, testBit)
import           Data.List               (foldl')
import           Data.Monoid             (Sum (..))

import           Data.Word               (Word64)

data Ternary = Z | O | X deriving (Eq)

instance Show Ternary where
    show Z = "0"
    show O = "1"
    show X = "X"

toTernary :: Bool -> Ternary
toTernary x = if x then O else Z

data Trie a
    = TV  !a
    | TZ  !(Trie a)
    | TO  !(Trie a)
    | TX  !(Trie a)
    | TZO !(Trie a) !(Trie a)
    deriving (Functor, Show)

instance Foldable Trie where
    foldMap f = \case
        TV x      -> f x
        TZ tz     -> foldMap f tz
        TO to     -> foldMap f to
        TX tx     -> let !s = foldMap f tx in s <> s
        TZO tz to -> foldMap f tz <> foldMap f to

singleton :: [Ternary] -> a -> Trie a
singleton []       x = TV x
singleton (Z : ks) x = TZ $ singleton ks x
singleton (O : ks) x = TO $ singleton ks x
singleton (X : ks) x = TX $ singleton ks x

insert :: [Ternary] -> a -> Trie a -> Trie a
insert []       x _           = TV x
insert _        x (TV _)      = TV x
insert (Z : ks) x (TZ tz)     = TZ (insert ks x tz)
insert (Z : ks) x (TO to)     = TZO (singleton ks x) to
insert (Z : ks) x (TX tx)     = TZO (insert ks x tx) tx
insert (Z : ks) x (TZO tz to) = TZO (insert ks x tz) to
insert (O : ks) x (TZ tz)     = TZO tz (singleton ks x)
insert (O : ks) x (TO to)     = TO (insert ks x to)
insert (O : ks) x (TX tx)     = TZO tx (insert ks x tx)
insert (O : ks) x (TZO tz to) = TZO tz (insert ks x to)
insert (X : ks) x (TZ tz)     = TZO (insert ks x tz) (singleton ks x)
insert (X : ks) x (TO to)     = TZO (singleton ks x) (insert ks x to)
insert (X : ks) x (TX tx)     = TX (insert ks x tx)
insert (X : ks) x (TZO tz to) = TZO (insert ks x tz) (insert ks x to)

type Program = [Instruction]

data Instruction
    = SetMask [Ternary]
    | SetMem !Word64 !Word64
    deriving (Show)

wordSize :: Int
wordSize = 36

wordToBinary :: Word64 -> [Bool]
wordToBinary w = [testBit w i | i <- [wordSize - 1, wordSize - 2 .. 0]]

binaryToWord :: [Bool] -> Word64
binaryToWord = foldl' (\acc x -> (acc `shiftL` 1) + if x then 1 else 0) 0

parseProgram :: P.Parser Char Program
parseProgram = P.sepBy (setMask <|> setMem) (P.char '\n')
  where
    setMask = SetMask . mkMask <$> (P.string "mask = " *> P.many1 mc)
    setMem = SetMem
        <$> (P.string "mem[" *> P.decimal <* P.string "] = ")
        <*> P.decimal
    mc = P.satisfy "mask" (`elem` "01X")
    mkMask = map (\case '0' -> Z; '1' -> O; _ -> X)

runProgram
    :: ([Ternary] -> Word64 -> Word64 -> ([Ternary], Word64))
    -> Program -> Word64
runProgram f = maybe 0 (getSum . foldMap Sum) . fst . foldl' step (Nothing, [])
  where
    step (tree, _) (SetMask m) = (tree, m)
    step (tree, mask) (SetMem a x) =
        let (ks, val) = f mask a x in
        (Just (maybe (singleton ks val) (insert ks val) tree), mask)

part1 :: Program -> Word64
part1 = runProgram $ \mask addr val ->
    ( map toTernary $ wordToBinary addr
    , binaryToWord $ zipWith f mask (wordToBinary val)
    )
  where
    f X a = a
    f Z _ = False
    f O _ = True

part2 :: Program -> Word64
part2 = runProgram $ \mask addr val ->
    (zipWith f mask . map toTernary $ wordToBinary addr, val)
  where
    f X _ = X
    f Z a = a
    f O _ = O

main :: IO ()
main = pureMain $ \input -> do
    program <- P.runParser parseProgram input
    pure (pure $ part1 program, pure $ part2 program)
