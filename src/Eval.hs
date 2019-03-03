{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Protolude hiding ((<|>), try, Type)
import Prelude (words)

import Data.Map as Map
import Control.Monad.Reader as Reader

import Parser
import Expr
import TypeCheck

data Value
    = VUnit
    | VInt Integer
    | VBool Bool
    | VFun [Char] Expr
  deriving (Show, Eq)

type ValContext = Map [Char] Value

runEval :: Expr -> Value
runEval e = runReader (eval e) Map.empty

eval :: Expr -> Reader ValContext Value
eval expr =

    case expr of

        EInt n          ->  pure $ VInt n

        EBool b         ->  pure $ VBool b

        EVar v          ->  do cxt <- Reader.ask
                               pure $ cxt ! v

        EBinOp Add x y  ->  do (VInt valX) <- eval x
                               (VInt valY) <- eval y
                               pure $ VInt (valX + valY)

        EBinOp Sub x y  ->  do (VInt valX) <- eval x
                               (VInt valY) <- eval y
                               pure $ VInt (valX - valY)

        EBinOp Mul x y  ->  do (VInt valX) <- eval x
                               (VInt valY) <- eval y 
                               pure $ VInt (valX * valY)
        
        EBinOp And x y  ->  do (VBool valX) <- eval x
                               (VBool valY) <- eval y
                               pure $ VBool (valX && valY)

        EBinOp Or x y   ->  do (VBool valX) <- eval x
                               (VBool valY) <- eval y
                               pure $ VBool (valX || valY)

        EBinOp XOr x y  ->  do (VBool valX) <- eval x
                               (VBool valY) <- eval y
                               pure $ VBool $ ((not valX) && valY) || (valX && (not valY))

        ECond b x y     ->  do (VBool valB) <- eval b
                               if valB then eval x
                               else eval y

        ELetTA str _ x y -> do valY <- eval x
                               local (insert str valY) (eval y)

        ELet str x y    ->  do valY <- eval x
                               local (insert str valY) (eval y)

        EFunTA x _ _ e  ->  pure $ VFun x e

        EFun x e        ->  pure $ VFun x e

        EFunApp f e     ->  do valE <- eval e
                               (VFun x fExpr) <- eval f
                               local (insert x valE) (eval fExpr)                                     


parseSolveEval :: [Char] -> Value
parseSolveEval str = 
  case parseExpr str of
    Left err -> panic $ show err
    Right expr -> let infM = infer expr
                      (_, _, consts) = runInfer nullTypeCxt infM
                      solveResult = solve $ Right (nullSubst, consts)
                  in case solveResult of
                        Left err -> panic $ show err
                        Right _ -> runEval expr

-- parseCheckEval :: [Char] -> Value
-- parseCheckEval str = 
--     case parseExpr str of
--         Left _ -> panic "there was an error while parsing"
--         Right expr -> case runTypecheck expr of
--                           Left err -> panic $ show err
--                           Right typ -> runEval expr


