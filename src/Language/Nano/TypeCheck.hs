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

typeCheck = error "TBD:typeCheck"

-- | Types of built-in operators and functions.
--   The parser turns  e1 + e2  into  EApp (EApp (EVar "+") e1) e2.

preludeTypes :: TypeEnv
preludeTypes =
  [
    (":",    error "TBD: :")
  , ("head", error "TBD: head")
  , ("tail", error "TBD: tail")
  , ("null", error "TBD: null")
  ]