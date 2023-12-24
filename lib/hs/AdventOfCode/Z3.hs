{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module AdventOfCode.Z3
    ( Sort (..)
    , Var
    , Expr
    , Program

    , mkVar

    , var
    , int

    , assert
    , checkSat
    , declareConst
    , declareConstEq
    , eval

    , toInt

    , add
    , sub
    , mul
    , div

    , (==)
    , (+)
    , (-)
    , (*)
    , (/)

    , render
    , run
    ) where

import           Data.Proxy       (Proxy (..))
import           Prelude          hiding (div, (*), (+), (-), (/), (==))
import Data.String (IsString (..))
import           System.Directory (getTemporaryDirectory)
import           System.FilePath  ((<.>), (</>))
import           System.Process   (readProcess)

data Sort = BoolSort | IntSort | RealSort | UnknownSort

class KnownSort (s :: Sort) where
    knownSort :: Proxy s -> Sort

instance KnownSort 'BoolSort where knownSort _ = BoolSort
instance KnownSort 'IntSort  where knownSort _ = IntSort
instance KnownSort 'RealSort where knownSort _ = RealSort

class IsArith (k :: Sort)
instance IsArith 'IntSort
instance IsArith 'RealSort

newtype Var (s :: Sort) = Var {unVar :: String}

instance IsString (Var s) where
    fromString = mkVar

data Expr (s :: Sort) where
    AppExpr :: Expr f -> [Expr 'UnknownSort] -> Expr s
    VarExpr :: Var s -> Expr s
    IntExpr :: Int -> Expr s
    SortExpr :: Sort -> Expr s

cast :: Expr a -> Expr b
cast (AppExpr f args)  = AppExpr f args
cast (VarExpr (Var v)) = VarExpr (Var v)
cast (IntExpr x)       = IntExpr x
cast (SortExpr s)      = SortExpr s

type Program = [Expr 'UnknownSort]

mkVar :: String -> Var s
mkVar = Var

var :: Var s -> Expr s
var = VarExpr

int :: IsArith n => Int -> Expr n
int = IntExpr

assert :: Expr 'BoolSort -> Program
assert x = [AppExpr (var "assert") [cast x]]

checkSat :: Program
checkSat = [AppExpr (var "check-sat") []]

declareConst :: forall s. KnownSort s => Var s -> Program
declareConst v =
    [AppExpr (var "declare-const") [cast (var v), SortExpr (knownSort proxy)]]
  where
    proxy = Proxy :: Proxy s

declareConstEq :: forall s. KnownSort s => Var s -> Expr s -> Program
declareConstEq v x = declareConst v <> assert (var v == x)

eval :: Expr a -> Program
eval x = [AppExpr (var "eval") [cast x]]

toInt :: IsArith n => Expr n -> Expr 'IntSort
toInt x = AppExpr (var "to_int") [cast x]

add :: IsArith n => [Expr n] -> Expr n
add args = AppExpr (var "+") (map cast args)

sub :: IsArith n => [Expr n] -> Expr n
sub args = AppExpr (var "-") (map cast args)

mul :: IsArith n => [Expr n] -> Expr n
mul args = AppExpr (var "*") (map cast args)

div :: IsArith n => [Expr n] -> Expr n
div args = AppExpr (var "/") (map cast args)

(==) :: Expr a -> Expr a -> Expr 'BoolSort
(==) x y = AppExpr (var "=") [cast x, cast y]
infix 4 ==

(+) :: IsArith n => Expr n -> Expr n -> Expr n
(+) x y = add [x, y]
infixl 6 +

(-) :: IsArith n => Expr n -> Expr n -> Expr n
(-) x y = sub [x, y]
infixl 6 -

(*) :: IsArith n => Expr n -> Expr n -> Expr n
(*) x y = mul [x, y]
infixl 7 *

(/) :: IsArith n => Expr n -> Expr n -> Expr n
(/) x y = div [x, y]
infixl 7 /

renderExpr :: Expr a -> String
renderExpr (AppExpr f args) = "(" <> renderExpr f <>
    concat [" " ++ renderExpr a | a <- args] <> ")"
renderExpr (VarExpr v) = unVar v
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
