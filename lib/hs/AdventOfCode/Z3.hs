{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module AdventOfCode.Z3
    ( Sort (..)
    , Var
    , Expr
    , Program

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
    , (.-)
    , (.*)
    , (./)

    , render
    , run
    ) where

import           System.Directory (getTemporaryDirectory)
import           System.FilePath  ((<.>), (</>))
import Data.Proxy (Proxy (..))
import           System.Process   (readProcess)

data Sort = BoolSort | IntSort | RealSort | UnknownSort

class KnownSort (s :: Sort) where
    knownSort :: Proxy s -> Sort

instance KnownSort 'BoolSort where knownSort _ = BoolSort
instance KnownSort 'IntSort  where knownSort _ = IntSort
instance KnownSort 'RealSort where knownSort _ = RealSort

type Var = String

data Expr (k :: Sort) where
    AppExpr :: Expr f -> [Expr 'UnknownSort] -> Expr a
    VarExpr :: Var -> Expr a
    IntExpr :: Int -> Expr a
    SortExpr :: Sort -> Expr a

cast :: Expr a -> Expr b
cast (AppExpr f args) = AppExpr f args
cast (VarExpr v)      = VarExpr v
cast (IntExpr x)      = IntExpr x
cast (SortExpr s)     = SortExpr s

type Program = [Expr 'UnknownSort]

class IsNum (k :: Sort)
instance IsNum 'IntSort
instance IsNum 'RealSort

var :: Var -> Expr a
var = VarExpr

int :: IsNum n => Int -> Expr n
int = IntExpr

assert :: Expr 'BoolSort -> Program
assert x = [AppExpr (var "assert") [cast x]]

checkSat :: Program
checkSat = [AppExpr (var "check-sat") []]

declareConst :: Var -> Sort -> Program
declareConst v ty =
    [AppExpr (var "declare-const") [var v, SortExpr ty]]

declareConstEq :: forall s. KnownSort s => Var -> Expr s -> Program
declareConstEq v x =
    declareConst v (knownSort proxy) <> assert (var v .= x)
  where
    proxy = Proxy :: Proxy s

eval :: Expr a -> Program
eval x = [AppExpr (var "eval") [cast x]]

toInt :: IsNum n => Expr n -> Expr 'IntSort
toInt x = AppExpr (var "to_int") [cast x]

(.=) :: Expr a -> Expr a -> Expr 'BoolSort
(.=) x y = AppExpr (var "=") [cast x, cast y]

(.+) :: IsNum n => [Expr n] -> Expr n
(.+) args = AppExpr (var "+") (map cast args)

(.-) :: IsNum n => [Expr n] -> Expr n
(.-) args = AppExpr (var "-") (map cast args)

(.*) :: IsNum n => [Expr n] -> Expr n
(.*) args = AppExpr (var "*") (map cast args)

(./) :: IsNum n => [Expr n] -> Expr n
(./) args = AppExpr (var "/") (map cast args)

renderExpr :: Expr a -> String
renderExpr (AppExpr f args) = "(" <> renderExpr f <>
    concat [" " ++ renderExpr a | a <- args] <> ")"
renderExpr (VarExpr v) = v
renderExpr (IntExpr x) = show x
renderExpr (SortExpr BoolSort) = "Bool"
renderExpr (SortExpr IntSort) = "Int"
renderExpr (SortExpr RealSort) = "Real"
renderExpr (SortExpr UnknownSort) = "Unknown"

render :: Program -> String
render = unlines . foldMap (pure . renderExpr)

run :: String -> Program -> IO String
run name z3 = do
    tmpDir <- getTemporaryDirectory
    let tmpPath = tmpDir </> name <.> ".z3"
    writeFile tmpPath $ render z3
    output <- readProcess "z3" [tmpPath] ""
    pure . last $ lines output
