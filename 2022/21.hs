import           AdventOfCode.Main
import           AdventOfCode.NanoParser as NP
import           Control.Applicative     (optional, (<|>))
import qualified Data.Map                as M
import           System.Directory        (getTemporaryDirectory)
import           System.FilePath         ((</>))
import           System.Process          (readProcess)

data BinOp = Add | Sub | Mul | Div deriving (Show)

data MonkeyExpr
    = Lit Int
    | Var String
    | BinOp MonkeyExpr BinOp MonkeyExpr
    deriving (Show)

type Monkeys = M.Map String MonkeyExpr

parseMonkeys :: NP.Parser Char Monkeys
parseMonkeys = M.fromList <$> NP.sepBy1 monkey NP.newline
  where
    monkey = (,) <$> (var <* NP.char ':' <* NP.spaces) <*> expr

    expr =
        (\lhs mbRhs -> case mbRhs of
            Nothing        -> lhs
            Just (op, rhs) -> BinOp lhs op rhs) <$>
        term <*> optional ((,) <$> binop <*> term)

    token p = p <* NP.horizontalSpaces
    term    = token $ (Var <$> var) <|> (Lit <$> NP.signedDecimal)
    var     = NP.many1 NP.lower
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

monkeysToZ3 :: M.Map String ExtendedExpr -> String -> String
monkeysToZ3 monkeys query = unlines $
    ["(declare-const " ++ k ++ " Int)" | (k, _) <- M.toList monkeys] ++
    (do
        (k, expr) <- M.toList monkeys
        (lhs, rhs) <- case expr of
                AssertEqual x y -> [(exprToZ3 x, exprToZ3 y)]
                Unknown         -> []
                MonkeyExpr e    -> [(k, exprToZ3 e)]
        pure $ "(assert (= " ++ lhs ++ " " ++ rhs ++ "))") ++
    ["(check-sat)", "(eval " ++ query ++ ")"]
  where
    exprToZ3 (Lit x) = show x
    exprToZ3 (Var v) = v
    exprToZ3 (BinOp x op y) =
        "(" ++ binopToZ3 op ++ " " ++ exprToZ3 x ++ " " ++ exprToZ3 y ++ ")"

    binopToZ3 Add = "+"
    binopToZ3 Sub = "-"
    binopToZ3 Mul = "*"
    binopToZ3 Div = "/"

runZ3 :: String -> IO String
runZ3 z3 = do
    tmpDir <- getTemporaryDirectory
    let tmpPath = tmpDir </> "aoc-2022-21.z3"
    writeFile tmpPath z3
    output <- readProcess "z3" [tmpPath] ""
    pure . last $ lines output

main :: IO ()
main = defaultMain $ \h -> do
    monkeys0 <- NP.hRunParser h parseMonkeys
    let part1 = monkeysToZ3 (fmap MonkeyExpr monkeys0) "root"
        monkeys1 = M.mapWithKey (\k e -> case (k, e) of
            ("root", BinOp x _ y) -> AssertEqual x y
            ("humn", _)           -> Unknown
            _                     -> MonkeyExpr e) monkeys0
        part2 = monkeysToZ3 monkeys1 "humn"
    pure (runZ3 part1 >>= putStrLn, runZ3 part2 >>= putStrLn)
