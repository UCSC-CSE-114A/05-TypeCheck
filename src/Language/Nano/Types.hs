-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Nano.Types where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import qualified Data.List as L

-- import           Control.Exception
-- import           Data.Typeable

-- import Data.Char (isAlpha, isAlphaNum, isDigit)
-- import Data.List (isInfixOf)
-- import Text.ParserCombinators.ReadP



--------------------------------------------------------------------------------
-- Types and expressions
--------------------------------------------------------------------------------

data Binop
  = Plus
  | Minus
  | Mul
  | Eq
  | Lt
  | Le
  | Ne
  | And
  | Or
  | Cons
  deriving (Eq,Ord)

data Type
  = TInt               -- Int
  | TBool              -- Bool
  | TFun Type Type     -- T1 -> T2
  | TIntList           -- [Int]
  deriving (Eq)

type Id = String

data Expr
  = EInt  Int
  | EBool Bool
  | ENil
  | EVar  Id
  | EBin  Binop Expr Expr
  | EIf   Expr Expr Expr
  | ELam  Id Type Expr          -- \(x : T) -> e
  | EApp  Expr Expr
  | ELet  Id Type Expr Expr     -- let x : t = e1 in e2   (recursive: x is in scope in e1)

data Value
  = VInt Int
  | VBool Bool
  | VNil
  | VPair Value Value
  | VErr String
  | VClos Env Id Expr           -- closure: environment, parameter, body
  | VPrim (Value -> Value)      -- built-in function

-- | The result of type checking: a type, or an error message
data TypeOrError = Ok Type | Error String
  deriving (Show)

-- | Type environment: maps variables to their types
type TypeEnv = [(Id, Type)]
-- | Environment: maps variables to their values  
type Env = [(Id, Value)]

instance Eq TypeOrError where
  (==) :: TypeOrError -> TypeOrError -> Bool
  (Ok t1) == (Ok t2) = t1 == t2
  Error _ == Error _ = True
  _       == _       = False

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (VInt x1)     == (VInt x2)     = x1 == x2
  (VBool x1)    == (VBool x2)    = x1 == x2
  VNil          == VNil          = True
  (VPair x1 y1) == (VPair x2 y2) = x1 == x2 && y1 == y2
  (VErr x)      == (VErr y)      = True
  _             == _             = False
  

  
{- Pretty printing -}  

instance Show Binop where
  show :: Binop -> String
  show = binopString

instance Show Value where
  show :: Value -> String
  show = valueString

instance Show Expr where
  show :: Expr -> String
  show = exprString

instance Show Type where
  show :: Type -> String
  show = typeString
  
instance IsString Expr where
  fromString :: String -> Expr
  fromString = EVar
  
binopString :: Binop -> String
binopString Plus  = "+"
binopString Minus = "-"
binopString Mul   = "*"
binopString Eq    = "=="
binopString Ne    = "!="
binopString Lt    = "<"
binopString And   = "&&"
binopString Or    = "||"
binopString Cons  = ":"
binopString Le    = "<="

valueString :: Value -> String
valueString (VInt i)        = printf "%d" i
valueString (VBool b)       = printf "%s" (show b)
valueString (VClos env x v) = printf "<<%s, \\%s -> %s>>" (envString env) x (show v)
valueString (VPair v w)     = printf "(%s : %s)" (show v) (show w)
valueString (VErr s)        = printf "ERROR: %s" s
valueString VNil            = "[]"
valueString (VPrim _)       = "<<primitive-function>>"

envString :: Env -> String
envString env = printf "{ %s }" (L.intercalate ", " bs)
  where
    bs        = [ x ++ " := " ++ show v | (x, v) <- env]

exprString :: Expr -> String
exprString (EInt i)       = printf "%d" i
exprString (EBool b)      = printf "%s" (show b)
exprString (EVar x)       = x
exprString (EBin o e1 e2) = printf "(%s %s %s)" (show e1) (show o) (show e2)
exprString (EIf c t e)    = printf "if %s then %s else %s" (show c) (show t) (show e)
exprString (ELet x t e e')  = printf "let %s : %s = %s in \n %s" x (show t) (show e) (show e')
exprString (EApp e1 e2)   = printf "((%s) %s)" (show e1) (show e2)
exprString (ELam x t e)     = printf "\\(%s : %s) -> %s" x (show t) (show e)
exprString ENil           = "[]"

typeString :: Type -> String
typeString (TInt)       = "Int"
typeString (TBool)      = "Bool"
typeString (TFun t1 t2) = printf "(%s) -> %s" (show t1) (show t2)
typeString (TIntList)   = "List Int"


--------------------------------------------------------------------------------
class Nano a where
  expr  :: a -> Expr
  value :: a -> Value

instance Nano Int where
  expr :: Int -> Expr
  expr  = EInt
  value :: Int -> Value
  value = VInt

instance Nano Bool where
  expr :: Bool -> Expr
  expr  = EBool
  value :: Bool -> Value
  value = VBool

exprList :: [Expr] -> Expr
exprList = foldr (EBin Cons) ENil

valueList :: [Value] -> Value
valueList = foldr VPair VNil
