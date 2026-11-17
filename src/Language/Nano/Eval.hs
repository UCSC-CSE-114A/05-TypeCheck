{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , safeExecFile
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

-- import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Language.Nano.TypeCheck

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (parseFile f >>= execExpr) -- `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseString s) -- `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) -- `catch` exitError

--------------------------------------------------------------------------------
safeExecExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
safeExecExpr e = return (typeSafeEval prelude e) -- `catch` exitError

--------------------------------------------------------------------------------
safeExecFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
safeExecFile f = (parseFile f >>= safeExecExpr) -- `catch` exitError


--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- True
--
-- >>> parse "False"
-- False
--
-- >>> parse "123"
-- 123
--

-- >>> parse "if x < 4 then a || b else a && b"
-- if (x < 4) then (a || b) else (a && b)
--
-- >>> parse "if 4 < z then 1 - z else 4 * z"
-- if (4 < z) then (1 - z) else (4 * z)
--
-- >>> parse "let a : Int = 6 * 2 in a /= 11"
-- let a : Int = (6 * 2) in 
--  (a != 11)
--
--

--
-- >>> parse "(((add3 (x)) y) z)"
-- ((((((add3) x)) y)) z)
--


-- >>> parse "1-2-3"
-- ((1 - 2) - 3)

-- >>> parse "1+a&&b||c+d*e-f-g x"
-- (((1 + a) && b) || (((c + (d * e)) - f) - ((g) x)))
--
-- >>> parse "1:3:5:[]"
-- (1 : (3 : (5 : [])))
--
-- >>> parse "[1,3,5]"
-- (1 : (3 : (5 : [])))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseString

exitError :: TypeOrError -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws a `VErr "unbound variable"` otherwise.
--

-- example environment
env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- ERROR: unbound variable
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId = error "TBD:lookupId"

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `VErr "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- TBD:eval
--
-- >>> eval env0 "p"
-- TBD:eval
--
-- part (b)
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- TBD:eval
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- TBD:eval
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- TBD:eval
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- TBD:eval
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- TBD:eval
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- TBD:eval
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" TInt (EInt 1) (ELet "y" TInt (EInt 2) e1)
-- >>> eval [] e2
-- TBD:eval
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" TInt (EBin Plus "x" "x")) (EInt 3))
-- TBD:eval
--
-- >>> let e3 = ELet "h" (TFun TInt TInt) (ELam "y" TInt (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" TInt (EInt 100) e3
-- >>> let e1 = ELet "f" (TFun TInt TInt) (ELam "g" (TFun TInt TInt) (ELet "x" TInt (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- TBD:eval
--
-- part (e)
-- |

-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- TBD:eval
-- >>> execExpr (EApp "head" el)
-- TBD:eval
-- >>> execExpr (EApp "tail" el)
-- TBD:eval
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval = error "TBD:eval"

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------

evalOp = error "TBD:evalOp"


prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
  ]


--------------------------------------------------------------------------------
typeSafeEval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
-- | `typeSafeEval e` should perform type-safe evaluation of an expression `e`.
--   First `typeSafeEval` should type-check the expression `e` (you may want to
--   use the preludeTypes defined in the TypeCheck.hs file).
--   If the expression is type-safe, then evaluate it and return the value.
--   Otherwise, return `VErr` with some error message.

typeSafeEval = error "TBD:typeSafeEval"