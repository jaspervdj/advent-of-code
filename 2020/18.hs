import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as P
import           Control.Applicative     ((<|>))

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show)

sym :: Char -> P.Parser Char ()
sym c = P.char c *> P.spaces

term :: P.Parser Char Expr -> P.Parser Char Expr
term rec = (Lit <$> P.decimal <* P.spaces) <|> (sym '(' *> rec <* sym ')')

expr1, expr2 :: P.Parser Char Expr
expr1 = P.chainl1 (term expr1) $ (Add <$ sym '+') <|> (Mul <$ sym '*')
expr2 = P.chainl1 (P.chainl1 (term expr2) (Add <$ sym '+')) (Mul <$ sym '*')

evalExpr :: Expr -> Int
evalExpr (Lit x)   = x
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Mul x y) = evalExpr x * evalExpr y

main :: IO ()
main = pureMain $ \input -> pure
    ( fmap (sum . map evalExpr) . traverse (P.runParser expr1) $ lines input
    , fmap (sum . map evalExpr) . traverse (P.runParser expr2) $ lines input
    )
