{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

-- import qualified Data.List as L
-- import           Text.Printf (printf)  
-- import           Control.Exception (throw)

--------------------------------------------------------------------------------
-- For printing the resulting type of an expression
--------------------------------------------------------------------------------

-- The initial type environment is empty
initTypeEnv :: TypeEnv
initTypeEnv = preludeTypes

typeOfFile :: FilePath -> IO TypeOrError
typeOfFile f = parseFile f >>= typeOfExpr

typeOfString :: String -> IO TypeOrError
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> IO TypeOrError
typeOfExpr e = return (typeCheck initTypeEnv e)


--------------------------------------------------------------------------------
-- Helper Functions for manipulating types/type environments
--------------------------------------------------------------------------------
  
-- | Look up a variable in a type environment
lookupVarType :: Id -> TypeEnv -> TypeOrError
lookupVarType x ((y, t) : gamma)
  | x == y    = Ok t
  | otherwise = lookupVarType x gamma
lookupVarType x [] = Error ("unbound variable: " ++ x)

-- | Extend a type environment with a new binding
extendTypeEnv :: Id -> Type -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

--------------------------------------------------------------------------------
-- TODO: the type checker
--------------------------------------------------------------------------------

-- the function you are implementing
typeCheck :: TypeEnv -> Expr -> TypeOrError

-- Problem 1: constants, variables, if
typeCheck _   (EInt _)  = Ok TInt
typeCheck _   (EBool _) = Ok TBool
typeCheck env (EVar x)  = lookupVarType x env
typeCheck env (EIf c e1 e2) =
  case (typeCheck env c, typeCheck env e1, typeCheck env e2) of
    (Error m, _, _)    -> Error m
    (_, Error m, _)    -> Error m
    (_, _, Error m)    -> Error m
    (Ok tc, Ok t1, Ok t2)
      | tc /= TBool    -> Error "type error"
      | t1 /= t2       -> Error "type error"
      | otherwise      -> Ok t1

-- Problem 2: Binary operations
typeCheck env (EBin op e1 e2) =
  case op of
    Plus  -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TInt) -> Ok TInt
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Minus -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TInt) -> Ok TInt
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Mul   -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TInt) -> Ok TInt
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Eq    -> case (typeCheck env e1, typeCheck env e2) of
                (Ok t1, Ok t2) -> if t1 == t2 then Ok TBool else Error "type error"
                (Error m, _) -> Error m
                (_, Error m) -> Error m
    Lt    -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TInt) -> Ok TBool
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Le    -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TInt) -> Ok TBool
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Ne    -> case (typeCheck env e1, typeCheck env e2) of
                (Ok t1, Ok t2) -> if t1 == t2 then Ok TBool else Error "type error"
                (Error m, _) -> Error m
                (_, Error m) -> Error m
    And   -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TBool, Ok TBool) -> Ok TBool
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Or    -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TBool, Ok TBool) -> Ok TBool
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"
    Cons  -> case (typeCheck env e1, typeCheck env e2) of
                (Ok TInt, Ok TIntList) -> Ok TIntList
                (Error m, _) -> Error m
                (_, Error m) -> Error m
                _            -> Error "type error"


-- Problem 3: functions, application, let

typeCheck env (ELam x t body) =
  case typeCheck (extendTypeEnv x t env) body of
    Error m -> Error m
    Ok tb   -> Ok (TFun t tb)

typeCheck env (EApp e1 e2) =
  case (typeCheck env e1, typeCheck env e2) of
    (Error m, _)  -> Error m
    (_, Error m)  -> Error m
    (Ok (TFun targ tres), Ok t2) -> if targ == t2 then Ok tres else Error "type error"
    (Ok _, Ok _)   -> Error "type error"

typeCheck env (ELet x t e1 e2) =
  let env' = extendTypeEnv x t env in
  case (typeCheck env' e1, typeCheck env' e2) of
    (Error m,_) -> Error m
    (_,Error m) -> Error m
    (Ok t1,Ok t2) -> if t == t1 then Ok t2 else Error "type error"


-- Problem 4: lists (of Int)
typeCheck _ ENil = Ok TIntList



-- | Types of built-in operators and functions.
--   The parser turns  e1 + e2  into  EApp (EApp (EVar "+") e1) e2.

preludeTypes :: TypeEnv
preludeTypes =
  [
  --   ("+",    TFun TInt (TFun TInt TInt))
  -- , ("-",    TFun TInt (TFun TInt TInt))
  -- , ("*",    TFun TInt (TFun TInt TInt))
  -- , ("==",   TFun TInt (TFun TInt TBool))
  -- , ("/=",   TFun TInt (TFun TInt TBool))
  -- , ("<",    TFun TInt (TFun TInt TBool))
  -- , ("<=",   TFun TInt (TFun TInt TBool))
  -- , ("&&",   TFun TBool (TFun TBool TBool))
  -- , ("||",   TFun TBool (TFun TBool TBool))
    (":",    TFun TInt (TFun TIntList TIntList))
  , ("head", TFun TIntList TInt)
  , ("tail", TFun TIntList TIntList)
  , ("null", TFun TIntList TBool)
  ]


      
      
-- -- | Types of built-in operators and functions      
-- preludeTypes :: TypeEnv
-- preludeTypes =
--   [ ("+",    Mono (TInt :=> TInt :=> TInt))
--   , ("-",    error "TBD: -")
--   , ("*",    error "TBD: *")
--   , ("/",    error "TBD: /")
--   , ("==",   error "TBD: ==")
--   , ("!=",   error "TBD: !=")
--   , ("<",    error "TBD: <")
--   , ("<=",   error "TBD: <=")
--   , ("&&",   error "TBD: &&")
--   , ("||",   error "TBD: ||")
--   , ("if",   error "TBD: if")
--   -- lists: 
--   , ("[]",   error "TBD: []")
--   , (":",    error "TBD: :")
--   , ("head", error "TBD: head")
--   , ("tail", error "TBD: tail")
--   ]
