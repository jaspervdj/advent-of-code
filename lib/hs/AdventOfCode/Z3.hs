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
    , maximize
    , minimize

    , not
    , ite

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

    , bv2int
    , int2bv

    , bvadd
    , bvmul
    , bvsdiv
    , bvsmod

    , bvsgt
    , bvslt

    , render
    , run
    ) where

import           Data.Proxy       (Proxy (..))
import           Prelude          hiding (div, (*), (+), (-), (/), (==), not)
import Data.String (IsString (..))
import           System.Directory (getTemporaryDirectory)
import           System.FilePath  ((<.>), (</>))
import           System.Process   (readProcess)
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat, natVal)

data Sort
    = BitVecSort Natural
    | BoolSort
    | IntSort
    | RealSort
    | UnknownSort

class KnownSort (s :: Sort) where
    knownSort :: Proxy s -> Sort

instance forall n. KnownNat n => KnownSort ('BitVecSort n) where
    knownSort _ = BitVecSort (fromIntegral $ natVal (Proxy :: Proxy n))

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

maximize :: Expr a -> Program
maximize x = [AppExpr (var "maximize") [cast x]]

minimize :: Expr a -> Program
minimize x = [AppExpr (var "minimize") [cast x]]

not :: Expr 'BoolSort -> Expr 'BoolSort
not x = AppExpr (var "not") [cast x]

ite :: Expr 'BoolSort -> Expr a -> Expr a -> Expr a
ite p x y = AppExpr (var "ite") [cast p, cast x, cast y]

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

bv2int :: Expr ('BitVecSort n) -> Expr 'IntSort
bv2int x = AppExpr (var "bv2int") [cast x]

int2bv :: forall n. KnownNat n => Expr 'IntSort -> Expr ('BitVecSort n)
int2bv x = AppExpr (AppExpr (var "_") [var "int2bv", IntExpr w]) [cast x]
  where
    w = fromIntegral $ natVal (Proxy :: Proxy n) :: Int

bvadd :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr ('BitVecSort n)
bvadd x y = AppExpr (var "bvadd") [cast x, cast y]

bvmul :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr ('BitVecSort n)
bvmul x y = AppExpr (var "bvmul") [cast x, cast y]

bvsdiv :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr ('BitVecSort n)
bvsdiv x y = AppExpr (var "bvsdiv") [cast x, cast y]

bvsmod :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr ('BitVecSort n)
bvsmod x y = AppExpr (var "bvsmod") [cast x, cast y]

bvsgt :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr 'BoolSort
bvsgt x y = AppExpr (var "bvsgt") [cast x, cast y]

bvslt :: Expr ('BitVecSort n) -> Expr ('BitVecSort n) -> Expr 'BoolSort
bvslt x y = AppExpr (var "bvslt") [cast x, cast y]

renderExpr :: Expr a -> String
renderExpr (AppExpr f args) = "(" <> renderExpr f <>
    concat [" " ++ renderExpr a | a <- args] <> ")"
renderExpr (VarExpr v) = unVar v
renderExpr (IntExpr x) = show x
renderExpr (SortExpr (BitVecSort n)) = "(_ BitVec " <> show n <> ")"
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
