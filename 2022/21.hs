{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
import           AdventOfCode.Main
import           AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Z3         as Z3
import           Control.Applicative     (optional, (<|>))
import           Data.Foldable           (toList)
import qualified Data.Map                as M

data BinOp = Add | Sub | Mul | Div deriving (Show)

data MonkeyExpr
    = Lit Int
    | Var String
    | BinOp MonkeyExpr BinOp MonkeyExpr
    deriving (Show)

type Monkeys = M.Map String MonkeyExpr

parseMonkeys :: NP.Parser Char Monkeys
parseMonkeys = M.fromList . toList <$> NP.sepBy1 monkey NP.newline
  where
    monkey = (,) <$> (var <* NP.char ':' <* NP.spaces) <*> expr

    expr =
        (\lhs mbRhs -> case mbRhs of
            Nothing        -> lhs
            Just (op, rhs) -> BinOp lhs op rhs) <$>
        term <*> optional ((,) <$> binop <*> term)

    token p = p <* NP.horizontalSpaces
    term    = token $ (Var <$> var) <|> (Lit <$> NP.signedDecimal)
    var     = toList <$> NP.many1 NP.lower
    binop   = token $
        (Add <$ NP.char '+') <|>
        (Sub <$ NP.char '-') <|>
        (Mul <$ NP.char '*') <|>
        (Div <$ NP.char '/')

-- | Extensions for part 2
data ExtendedExpr
    = MonkeyExpr MonkeyExpr
    | Unknown
    | AssertEqual MonkeyExpr MonkeyExpr
    deriving (Show)

monkeysToZ3 :: M.Map String ExtendedExpr -> Z3.Var 'Z3.IntSort -> Z3.Program
monkeysToZ3 monkeys query = mconcat $
    [Z3.declareConst (mvar k) | (k, _) <- M.toList monkeys] ++
    (do
        (k, expr) <- M.toList monkeys
        (lhs, rhs) <- case expr of
                AssertEqual x y -> [(exprToZ3 x, exprToZ3 y)]
                Unknown         -> []
                MonkeyExpr e    -> [(Z3.var (mvar k), exprToZ3 e)]
        pure $ Z3.assert $ lhs Z3.== rhs) <>
    [Z3.checkSat, Z3.eval (Z3.var query)]
  where
    exprToZ3 :: MonkeyExpr -> Z3.Expr 'Z3.IntSort
    exprToZ3 (Lit x)         = Z3.int x
    exprToZ3 (Var v)         = Z3.var (mvar v)
    exprToZ3 (BinOp x Add y) = exprToZ3 x Z3.+ exprToZ3 y
    exprToZ3 (BinOp x Sub y) = exprToZ3 x Z3.- exprToZ3 y
    exprToZ3 (BinOp x Mul y) = exprToZ3 x Z3.* exprToZ3 y
    exprToZ3 (BinOp x Div y) = exprToZ3 x Z3./ exprToZ3 y

    mvar :: String -> Z3.Var 'Z3.IntSort
    mvar = Z3.mkVar

main :: IO ()
main = defaultMain $ \h -> do
    monkeys0 <- NP.hRunParser h parseMonkeys
    let part1 = monkeysToZ3 (fmap MonkeyExpr monkeys0) "root"
        monkeys1 = M.mapWithKey (\k e -> case (k, e) of
            ("root", BinOp x _ y) -> AssertEqual x y
            ("humn", _)           -> Unknown
            _                     -> MonkeyExpr e) monkeys0
        part2 = monkeysToZ3 monkeys1 "humn"
    pure (Z3.run "part1" part1 >>= putStrLn, Z3.run "part2" part2 >>= putStrLn)
