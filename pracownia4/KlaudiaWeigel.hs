-- Pracownia 4, Metody Programowania.
-- Autor: Klaudia Weigel

{-# LANGUAGE Safe #-}

module KlaudiaWeigel (typecheck, eval) where

import AST
import DataTypes

-- możliwe typy: Bool, Integer lub Error
data Type = TBool | TInt | TError deriving (Eq, Show)

-- środowisko wykorzystywane przez typecheck
type Env = [(Var, Type)]

typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck vars expr = typecheck_helper (create_env_typecheck vars) expr

-- Tworzy środowisko dla funkcji typecheck_helper
create_env_typecheck :: [Var] -> Env
create_env_typecheck (x:xs) = (x, TInt) : create_env_typecheck xs
create_env_typecheck [] = []

-- sprawdza czy zmienna jest w środowisku
is_in_env :: Env -> Var -> Bool
is_in_env ((a, b):xs) v = if a==v then True else is_in_env xs v
is_in_env [] _ = False

-- rozszerzenie śr. o zmienną v
extend_env :: Env -> (Var, Type) -> Env
extend_env vars v = v:vars

typecheck_helper::Env -> Expr p -> TypeCheckResult p
typecheck_helper env (EVar p v) =
    if (is_in_env env v) then Ok
    else Error p ("Undefined variable: " ++ v)
typecheck_helper env (ENum p n) = Ok
typecheck_helper env (EBool p b) = Ok
typecheck_helper env (EUnary p op e1) 
    | op==UNeg && typ==TInt = Ok
    | op==UNot && typ==TBool = Ok
    | otherwise = Error p ("Type Error")
  where
    typ = infer_type env e1
typecheck_helper env (EBinary p op e1 e2)
    | (op==BNeq || op==BEq  || op==BLe ) && typ1==TInt  && typ2==TInt  = Ok
    | (op==BGe  || op==BGt  || op==BLt ) && typ1==TInt  && typ2==TInt  = Ok
    | (op==BMod || op==BMul || op==BDiv) && typ1==TInt  && typ2==TInt  = Ok
    | (op==BSub || op==BAdd)             && typ1==TInt  && typ2==TInt  = Ok
    | (op==BAnd || op==BOr )             && typ1==TBool && typ2==TBool = Ok
    | otherwise = Error p ("Type Error")
  where
    typ1 = infer_type env e1
    typ2 = infer_type env e2
    
typecheck_helper env (ELet p variable e1 e2) =
    let type1 = (infer_type env e1) in
    if type1==TError then Error p "Wrong type"
    else 
        if infer_type (extend_env env (variable, type1)) e2 == TInt then Ok
        else Error p "Wrong type"
typecheck_helper env (EIf p e1 e2 e3) =
    let type1 = infer_type env e1
        type2 = infer_type env e2
        type3 = infer_type env e3
    in 
        if type1==TBool && type2==type3 then Ok
        else Error p "Wrong type"

-- Fukcja wyprowadzająca typy wyrażeń           
infer_type :: Env -> Expr p -> Type
infer_type env (EVar p v) =
    let wynik = lookup_type env v in
        if wynik /= TError then wynik 
        else TError
infer_type env (ENum p n) = TInt
infer_type env (EBool p n) = TBool
infer_type env (EUnary p op e) 
    | op == UNot && (infer_type env e) == TBool = TBool
    | op == UNeg && (infer_type env e) == TInt = TInt
    | otherwise = TError
infer_type env (EBinary p op e1 e2)
    | op == BAnd && typ1==TBool && typ2==TBool = TBool
    | op == BOr && typ1==TBool && typ2==TBool = TBool
    | op == BNeq && typ1==TInt && typ2==TInt = TBool
    | op == BLe && typ1==TInt && typ2==TInt = TBool
    | op == BGe && typ1==TInt && typ2==TInt = TBool
    | op == BEq && typ1==TInt && typ2==TInt = TBool
    | op == BGt && typ1==TInt && typ2==TInt = TBool
    | op == BLt && typ1==TInt && typ2==TInt = TBool
    | (op==BAdd ||  op==BSub ||  op==BMul || op==BDiv || op==BMod ) && typ1==TInt && typ2==TInt = TInt
    | otherwise = TError
 where
    typ1 = infer_type env e1
    typ2 = infer_type env e2
            
infer_type env (ELet p variable e1 e2) =
    let type1 = infer_type env e1
    in 
        if type1 /= TError then infer_type (extend_env env (variable, type1)) e2
        else TError
            
infer_type env (EIf p e1 e2 e3) =
    let type1 = infer_type env e1
        type2 = infer_type env e2
        type3 = infer_type env e3
    in 
        if (type1 /= TBool || type2 /= type3) then TError
        else type2

-- Zwraca typ zmiennej v
lookup_type :: Env -> Var -> Type
lookup_type ((a, b):xs) v = if a==v then b else lookup_type xs v
lookup_type [] _ = TError


data Val = IVal Integer | BVal Bool | Err deriving (Eq, Show)

-- Środowisko dla funkcji eval
type EnvEval = [(Var, Val)]

create_env_eval :: [(Var, Integer)] -> EnvEval
create_env_eval ((v, val):vars) = (v, IVal val) : (create_env_eval vars)
create_env_eval []=[]

add_to_env :: EnvEval -> (Var, Val) -> EnvEval
add_to_env vars v = v:vars

lookup_var :: [(Var, Val)] -> Var -> Val
lookup_var ((a, b):xs) v = if a==v then b else lookup_var xs v

to_int :: Val->Integer
to_int (IVal x) = x

to_bool :: Val ->  Bool
to_bool (BVal x) = x

eval :: [(Var, Integer)] -> Expr p -> EvalResult
eval vars expr = if res /= Err && res /= BVal False && res /= BVal True
                    then Value (to_int res)
                 else RuntimeError
            where res = eval_helper (create_env_eval vars) expr

eval_helper :: EnvEval -> Expr p -> Val
eval_helper env (ENum p n) = (IVal n)
eval_helper env (EBool p b) = (BVal b)
eval_helper env (EVar p v) = lookup_var env v
eval_helper env (EBinary p BDiv e1 e2) =
    let num1 = (eval_helper env e1)
        num2 = (eval_helper env e2)
    in
       if (to_int num2) /= 0 then (IVal ((to_int num1) `div` (to_int num2)))
       else Err
        
eval_helper env (EBinary p op e1 e2) =
    let num1 = (eval_helper env e1)
        num2 = (eval_helper env e2)
    in 
        case op of
            BAnd -> (BVal ((to_bool num1) && (to_bool num2)))
            BOr -> (BVal ((to_bool num1) || (to_bool num2)))
            BEq -> (BVal ((to_int num1) == (to_int num2)))
            BNeq -> (BVal ((to_int num1) /= (to_int num2)))
            BGe -> (BVal ((to_int num1) >= (to_int num2)))
            BLe -> (BVal ((to_int num1) <= (to_int num2)))
            BLt -> (BVal ((to_int num1) < (to_int num2)))
            BGt -> (BVal ((to_int num1) > (to_int num2)))
            BSub -> (IVal ((to_int num1) - (to_int num2)))
            BAdd -> (IVal ((to_int num1) + (to_int num2)))
            BMul -> (IVal ((to_int num1) * (to_int num2)))
            BMod -> (IVal ((to_int num1) `mod` (to_int num2)))
            
eval_helper env (ELet p variable e1 e2) = 
    eval_helper (add_to_env env (variable, (eval_helper env e1))) e2
    
eval_helper env (EIf p e1 e2 e3) =
    if (to_bool (eval_helper env e1)) then eval_helper env e2
    else eval_helper env e3
    
eval_helper env (EUnary p op e) =
    let num1 = eval_helper env e
    in
        case op of
            UNeg -> (IVal (-(to_int num1)))
            UNot -> (BVal ( not (to_bool num1)))
