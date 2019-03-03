{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheck where

import Protolude hiding ((<|>), try, Type, Constraint)
import Prelude (words)

import Data.Map as Map
import Data.List as List
import Data.Set as Set
import Control.Monad.Reader as Reader
import Control.Monad.Writer.Strict as Writer
import Control.Monad.RWS.Strict as RWS

import Data.Typeable
import Debug.Trace as DT

import Parser
import Expr

type CheckResult = Either Error Type

data Error 
    = Mismatch [Char] 
    | InvalidCond 
    | InvalidLet 
    | InvalidFun
    | VariableNonExistent [Char]
    | UnificationFail Type Type
    | InfiniteType
  deriving (Show, Eq)

data TypeContext = TypeContext (Map [Char] Scheme)

nullTypeCxt :: TypeContext
nullTypeCxt = TypeContext $ Map.empty

extend :: TypeContext -> [Char] -> Scheme -> TypeContext
extend (TypeContext cxt) x sch = TypeContext (Map.insert x sch cxt)

remove :: TypeContext -> [Char] -> TypeContext
remove (TypeContext cxt) x = TypeContext (Map.delete x cxt)

-- insertTypeCxt :: [Char] -> Type -> TypeContext -> TypeContext
-- insertTypeCxt str ty (TypeContext cxt) = TypeContext $ Map.insert str ty cxt

lookupVarType :: [Char] -> InferMonad Type
lookupVarType v = do
  (TypeContext cxt) <- Reader.ask
  case Map.lookup v cxt of
      Nothing -> panic "no type for this type variable"
      -- gives fresh variable type variables in case of conflicts in the cxt
      Just sch@(Forall tvars t) -> DT.trace "lookup var instantiate call" $ instantiate sch

extendCxt :: [([Char], Scheme)] -> InferMonad a -> InferMonad a
extendCxt schemes = do
    -- remove occurence of x from the type context, replace with new type
    let extCxt [] e = e
        extCxt ((x, sch):rest) e = extCxt rest newCxt
            where newCxt :: TypeContext
                  newCxt = extend (remove e x) x sch
    local (extCxt schemes)

---------------------------------------------------------------------
-- TYPE INFERENCE
---------------------------------------------------------------------

type InferMonad a = RWS TypeContext [Constraint] CounterState a

runInfer :: TypeContext -> InferMonad a -> (a, CounterState, [Constraint])
runInfer cxt infM = runRWS infM cxt startCounter

data CounterState = CounterState { count :: Int }
  deriving (Show, Eq)

startCounter :: CounterState
startCounter = CounterState { count = 0 }

letters :: [[Char]]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: InferMonad Type
fresh = do
    st <- get
    modify (\s -> s {count = count st + 1})
    return . TVar $ TV (letters List.!! count st)

data Constraint = Constraint Type Type 
  deriving (Show, Eq)

genConstraint :: Type -> Type -> InferMonad ()
genConstraint t1 t2 = tell [Constraint t1 t2]

infer :: Expr -> InferMonad Type
infer expr = 

    case expr of
    
        EBool b             ->   pure TBool
        
        EInt n              ->   pure TInt

        EVar v              ->   lookupVarType v

        EBinOp op e1 e2     ->   do type1 <- infer e1
                                    type2 <- infer e2
                                    genConstraint type1 type2
                                    if op `elem` [Add, Sub, Mul]
                                    then genConstraint type1 TInt
                                    else genConstraint type1 TBool
                                    pure type1

        ECond e1 e2 e3      ->   do type1 <- infer e1
                                    type2 <- infer e2
                                    type3 <- infer e3
                                    genConstraint type1 TBool
                                    genConstraint type2 type3
                                    pure type2

        ELetTA x t ex e     ->   do typeX <- infer ex 
                                    genConstraint typeX t
                                    extendCxt [(x, Forall [] t)] (infer e)

        ELet x (EFun a b) e ->   do typeF <- infer $ EFun a b
                                    cxt <- ask
                                    let sch = generalize cxt typeF
                                    extendCxt [(x, sch)] (infer e)

        ELet x ex e         ->   do xt <- fresh
                                    typeX <- infer ex 
                                    genConstraint typeX xt
                                    extendCxt [(x, Forall [] xt)] (infer e)

        EFunTA x xt rt e    ->   do typeExpr <- extendCxt [(x, Forall [] xt)] (infer e)
                                    genConstraint rt typeExpr
                                    pure $ TFun xt rt

        EFun x e            ->   do xt <- fresh
                                    rt <- extendCxt [(x, Forall [] xt)] (infer e)
                                    pure $ TFun xt rt

        EFunApp f x         ->   do typeX <- infer x
                                    typeF <- infer f
                                    a <- fresh
                                    b <- fresh
                                    genConstraint typeF (TFun a b)
                                    genConstraint typeX a
                                    pure b


---------------------------------------------------------------------
-- SUBSTITUTION
---------------------------------------------------------------------

type Substitution = Map.Map TypeVar Type

nullSubst :: Substitution
nullSubst = Map.empty

type Unifier = Either Error (Substitution, [Constraint])

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> Set.Set TypeVar

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = List.foldr (Set.union . ftv) Set.empty

instance Substitutable Type where
  apply s t@(TVar a)   = Map.findWithDefault t a s
  apply s (TFun t1 t2) = apply s t1 `TFun` apply s t2
  apply _ t = t

  ftv (TVar a)     = Set.singleton a
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2 -- this defines whether we have 
  ftv t            = Set.empty                 -- lexical or dynamic scoping???

-- Map.map :: (a -> b) -> Map k a -> Map k b
-- where a must be [Char] and b must be Scheme in this case
instance Substitutable TypeContext where
  apply s (TypeContext cxt) = TypeContext $ Map.map (apply s) cxt
  ftv (TypeContext cxt) = ftv $ Map.elems cxt

instance Substitutable Constraint where
  apply sub c@(Constraint t t') = Constraint (apply sub t) (apply sub t')
  ftv (Constraint t1 t2) = Set.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
    apply sub (Forall tvars ts) = Forall tvars $ apply (List.foldr Map.delete sub tvars) ts
    ftv (Forall tvars ts) = notImplemented

-- takes constraint and tries to unify the two types in the constraint
unify :: Type -> Type -> Unifier
unify (TVar v) t = bindVars v t
unify t (TVar v) = bindVars v t
unify (TFun a b) (TFun c d) = Right (nullSubst, [Constraint a c, Constraint b d])
unify a b | a == b = Right (nullSubst, [])
unify a b = Left $ UnificationFail a b

-- solve unifies single constraint in list of constraints?
solve :: Unifier -> Either Error Substitution
solve (Right (sub, [])) = Right sub
solve (Right (sub, (Constraint t1 t2):consts)) =
  case unify t1 t2 of
    Right (newSub, c) -> solve $ Right (composeSubsts newSub sub, c ++ apply newSub consts)
    Left err -> Left err
solve (Left err) = Left err

composeSubsts :: Substitution -> Substitution -> Substitution
composeSubsts a b = Map.map (apply a) b `Map.union` a

-- occurs check premise: type variable a must not appear in free 
-- variables of type t when being substituted
-- function asks for free variables of a, and checks for t's membership
occursCheck :: (Substitutable a) => TypeVar -> a -> Bool
occursCheck t s = t `Set.member` ftv s

bindVars :: TypeVar -> Type -> Unifier
bindVars v t = if occursCheck v t 
               then Left InfiniteType
               else Right $ (Map.singleton v t, [])


---------------------------------------------------------------------
-- POLYMORPHISM
---------------------------------------------------------------------

-- instead of calculating types for things, and then type checking,
-- we substitute new type variables and then type check the expanded
-- expression by making constraints...etc


data Scheme = Forall [TypeVar] Type
  deriving (Show, Eq)

instantiate :: Scheme -> InferMonad Type
instantiate (Forall vs t) = do
  vs' <- DT.trace "replicate" $ replicateM (List.length vs) fresh --supposed to be a list of TypeVars
  let s = DT.trace "fromList" $ Map.fromList $ zip vs vs'
  return $ DT.trace "return apply call" $ apply s t

-- only generalize variables that aren't mentioned
-- in the type environment
generalize :: TypeContext -> Type -> Scheme
generalize env t  = Forall vs t
    where vs = Set.toList $ ftv t `Set.difference` ftv env


---------------------------------------------------------------------
-- PARSE INPUT TESTER FUNCTIONS
---------------------------------------------------------------------

parseInfer :: [Char] -> (Type, CounterState, [Constraint])
parseInfer str = 
    case parseExpr str of
        Left err -> panic $ show err
        Right expr -> do let infM = infer expr 
                         runInfer nullTypeCxt infM

parseSolve :: [Char] -> Either Error Substitution
parseSolve str = 
    let (t, st, consts) = parseInfer str in
    solve $ Right (nullSubst, consts)


