module AdventOfCode.Z3
    ( Program
    , Type (..)
    , Var
    , Expr (..)

    , var
    , int

    , assert
    , checkSat
    , declareConst
    , declareConstEq
    , eval

    , toInt

    , (.=)
    , (.+)
    , (.*)


    , render
    , run
    ) where

import           System.Directory (getTemporaryDirectory)
import           System.FilePath  ((<.>), (</>))
import           System.Process   (readProcess)

type Program = [Expr]

data Type = RealType

type Var = String

data Expr
    = AppExpr Expr [Expr]
    | VarExpr Var
    | IntExpr Int
    | TypeExpr Type

var :: Var -> Expr
var = VarExpr

int :: Int -> Expr
int = IntExpr

assert :: Expr -> Program
assert x = [AppExpr (var "assert") [x]]

checkSat :: Program
checkSat = [AppExpr (var "check-sat") []]

declareConst :: Var -> Type -> Program
declareConst v ty =
    [AppExpr (var "declare-const") [var v, TypeExpr ty]]

declareConstEq :: Var -> Type -> Expr -> Program
declareConstEq v ty x = declareConst v ty <> assert ((.=) (var v) x)

eval :: Expr -> Program
eval x = [AppExpr (var "eval") [x]]

toInt :: Expr -> Expr
toInt x = AppExpr (var "to_int") [x]

(.=) :: Expr -> Expr -> Expr
(.=) x y = AppExpr (var "=") [x, y]

(.+) :: [Expr] -> Expr
(.+) args = AppExpr (var "+") args

(.*) :: [Expr] -> Expr
(.*) args = AppExpr (var "*") args

render :: Program -> String
render = unlines . map renderExpr
  where
    renderExpr (AppExpr f args) = "(" <> renderExpr f <>
        concat [" " ++ renderExpr a | a <- args] <> ")"
    renderExpr (VarExpr v) = v
    renderExpr (IntExpr x) = show x
    renderExpr (TypeExpr RealType) = "Real"

run :: String -> Program -> IO String
run name z3 = do
    tmpDir <- getTemporaryDirectory
    let tmpPath = tmpDir </> name <.> ".z3"
    writeFile tmpPath $ render z3
    output <- readProcess "z3" [tmpPath] ""
    pure . last $ lines output
