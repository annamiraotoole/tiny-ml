{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Expr where

import Protolude hiding (Type, Constraint)

import Data.Map as Map

--import Data.Map

data TypeVar = TV [Char] --stores name of type variable
  deriving (Show, Eq, Ord)

data Type
    = TVar TypeVar
    | TBool
    | TInt
    | TFun Type Type
  deriving (Show, Eq)

data Expr 
    = EInt Integer
    | EBool Bool
    | EVar [Char]
    | EBinOp BinOp Expr Expr
    | ECond Expr Expr Expr
--- | syntax: let x : type1 = <definition> in <other expression>
--- | order: var name, var type, definition, other expression
    | ELetTA [Char] Type Expr Expr
    | ELet [Char] Expr Expr
--- | syntax: (fun (x : type1) : type2 = <function expression>)
--- | order: var name, var type, return type, expression
    | EFunTA [Char] Type Type Expr
--- | order: var name, body expression
    | EFun [Char] Expr
    | EFunApp Expr Expr
  deriving (Show, Eq)

data BinOp = Add | Sub | Mul | And | Or | XOr
  deriving (Show, Eq)

makeBinOp :: [Char] -> Expr -> Expr -> Expr
makeBinOp str = 
    case str of
        "+" -> EBinOp Add
        "-" -> EBinOp Sub
        "*" -> EBinOp Mul
        "AND" -> EBinOp And
        "OR" -> EBinOp Or
        "XOR" -> EBinOp XOr
        _ -> panic "invalid BinOp"
