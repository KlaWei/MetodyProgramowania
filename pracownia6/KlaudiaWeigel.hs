-- Pracownia 6, Metody Programowania.
-- Autor: Klaudia Weigel

 {-# LANGUAGE Safe #-}

module KlaudiaWeigel (typecheck, eval) where

import AST
import DataTypes

-- środowisko wykorzystywane przez typecheck
type Env = [(Var, Type)]

data TypeOperator = TypeArith | TypeBoolean | TypeComp deriving (Eq, Show)
data Error p = Err p String deriving (Eq, Show)


typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p

typecheck fs vars expr =
  let env = add_functions_to_env fs (create_env_typecheck vars)
      func_types = typecheck_functions fs env
  in
    case func_types of
      Ok -> case infer_type env expr of
              Right TInt -> Ok
              Right t -> Error (getData expr) ("wrong result type: " ++ show t)
              Left (Err p m) -> Error p m
      Error p m -> Error p m

create_env_typecheck :: [Var] -> Env
create_env_typecheck (x:xs) = (x, TInt) : create_env_typecheck xs
create_env_typecheck [] = []

add_functions_to_env :: [FunctionDef p] -> Env -> Env
add_functions_to_env (f:fs) env = add_functions_to_env fs (env ++ [(funcName f, TArrow (funcArgType f) (funcResType f))])
add_functions_to_env [] env = env

-- sprawdza czy zmienna jest w środowisku
is_in_env :: Env -> Var -> Bool
is_in_env ((a, b):xs) v = if a==v then True else is_in_env xs v
is_in_env [] _ = False


-- rozszerzenie śr. o zmienną v
extend_env :: Env -> (Var, Type) -> Env
extend_env vars v = v:vars

operator_type :: BinaryOperator -> TypeOperator
operator_type op | op == BAnd = TypeBoolean
                 | op == BOr  = TypeBoolean
                 | op == BNeq = TypeComp
                 | op == BLe  = TypeComp
                 | op == BGe  = TypeComp
                 | op == BEq  = TypeComp
                 | op == BGt  = TypeComp
                 | op == BLt  = TypeComp
                 | (op==BAdd ||  op==BSub ||  op==BMul || op==BDiv || op==BMod ) = TypeArith


typecheck_functions :: [FunctionDef p] -> Env -> TypeCheckResult p
typecheck_functions (f:fs) env = 
  case infer_type (extend_env env ((funcArg f), (funcArgType f))) (funcBody f) of
    Right t -> if t==(funcResType f) then typecheck_functions fs env
               else Error (getData (funcBody f)) "wrong function type"
    Left (Err p n) -> Error p n

typecheck_functions [] env = Ok

infer_type :: Env -> Expr p  -> Either (Error p) Type
infer_type env (EVar p v) =
    case (lookup_type env v) of
        Left False -> Left (Err p "undefined variable")
        Right t -> Right t
          
infer_type env (ENum p n) = Right TInt
infer_type env (EBool p n) = Right TBool
infer_type env (EUnary p op e) =
    case op of
        UNot -> case (infer_type env e) of
                    Right TBool -> Right TBool
                    Right t -> Left (Err p "wrong type")
                    Left err -> Left err
        UNeg -> case (infer_type env e) of
                    Right TInt -> Right TInt
                    Right t -> Left (Err p "wrong type")
                    Left err ->Left err


infer_type env (EBinary p op e1 e2) =
    let typ1 = infer_type env e1
        typ2 = infer_type env e2
    in
     case operator_type op of
      TypeBoolean -> case typ1 of
                      Right TBool -> case typ2 of
                                      Right TBool -> Right TBool
                                      Right _ -> Left (Err p "type error")
                                      Left err -> Left err
                      Right _ -> Left (Err p "type error")
                      Left err -> Left err
      TypeArith -> case typ1 of
                    Right TInt -> case typ2 of
                                   Right TInt -> Right TInt
                                   Right _ -> Left (Err p "type error")
                                   Left err -> Left err
                    Right _ -> Left (Err p "type error")
                    Left err -> Left err

      TypeComp -> case typ1 of
                    Right TInt -> case typ2 of
                                   Right TInt -> Right TBool
                                   Right _ -> Left (Err p "type error")
                                   Left err -> Left err
                    Right _ -> Left (Err p "type error")
                    Left err -> Left err
            
infer_type env (ELet p variable e1 e2) =
    case (infer_type env e1) of
        Right t -> infer_type (extend_env env (variable, t)) e2
        Left err -> Left (Err p "type error")

infer_type env (EIf p e1 e2 e3) =
     let type1 = (infer_type env e1)
         type2 = (infer_type env e2)
         type3 = (infer_type env e3)
     in 
        case type1 of
            Right TBool -> case type2 of
                            Right t -> case type3 of
                                         Right t2 -> if t==t2 then Right t else Left (Err p "error type")
                                         Left err -> Left err
                            Left err -> Left err
            Right _ -> Left (Err p "type error")
            Left err -> Left err

infer_type env (EFn p v vartype e1) =
  case infer_type (extend_env env (v, vartype)) e1 of
    Right t -> Right (TArrow vartype t)
    Left err -> Left err

infer_type env (EApp p e1 e2) = 
  let res1 = infer_type env e1
      res2 = infer_type env e2
  in
    case res1 of
      Right (TArrow t1 t2) ->
        case res2 of
          Right t -> if t1==t then Right t2
                     else Left (Err p "type error")
          Left err -> Left err
      Right t1 -> Left (Err p "type error")
      Left err -> Left err

infer_type env (EUnit p) = Right TUnit
infer_type env (EPair p e1 e2) =
    case (infer_type env e1) of
        Right t1 -> case (infer_type env e2) of
                        Right t2 -> Right (TPair t1 t2)
                        Left err -> Left err
        Left err -> Left err
infer_type env (EFst p e1) =
    case (infer_type env e1) of
        Right (TPair t1 t2) -> Right t1
        Left err -> Left err
infer_type env (ESnd p e1) =
    case (infer_type env e1) of
        Right (TPair t1 t2) -> Right t2
        Left err -> Left err
infer_type env (ENil p t) = Right t
infer_type env (ECons p e1 e2) =
    case (infer_type env e1) of
        Right t -> case (infer_type env e2) of
                       Right (TList t2) -> if t==t2 then Right (TList t2)
                                           else Left (Err p "type error")
                       Left err -> Left err
        Left err -> Left err
        
infer_type env (EMatchL p expr nclause (x1, x2, e2)) =
    let typelist = infer_type env expr
        typenclause = infer_type env nclause
        cexprtype = infer_type env e2
    in
      case typelist of
        Right (TList t) -> case (infer_type (extend_env (extend_env env (x1, t)) (x2, (TList t))) e2) of
                             Right typ -> case typenclause of
                                            Right typ2 -> if typ==typ2 then Right typ
                                                          else Left (Err p "type error")
                                            Left err -> Left err
                             Left err -> Left err
        Right _ -> Left (Err p "type error")
        Left err -> Left err

lookup_type :: Env -> Var -> Either (Bool) Type
lookup_type ((a, b):xs) v = if a==v then (Right b) else lookup_type xs v
lookup_type [] _ = Left False

data Val p =  IVal Integer
            | BVal Bool 
            | UVal
            | PVal (Val p, Val p) 
            | LVal [Val p] 
            | AVal (Val p) (Val p) 
            | ClosureVal [FunctionDef p] (FunctionDef p)
            | ClosureLambda (EnvEval p) (Expr p) Var 
            | ErrVal
    deriving (Eq, Show)
type EnvEval p = [(Var, Val p)]

add_functions_to_env_eval :: [FunctionDef p] -> EnvEval p -> EnvEval p
add_functions_to_env_eval (f:fs) env = 
  add_functions_to_env_eval fs (env ++ [((funcName f), (ClosureVal (f:fs) f))])
add_functions_to_env_eval [] env = env

eval :: [FunctionDef p] -> [(Var, Integer)] -> Expr p -> EvalResult

eval fs vars e = 
  let res = eval_helper (add_functions_to_env_eval fs (create_env_eval vars)) e
  in 
    case res of
      IVal i -> Value i
      ErrVal -> RuntimeError


create_env_eval :: [(Var, Integer)] -> EnvEval p
create_env_eval ((v, val):vars) = (v, IVal val) : (create_env_eval vars)
create_env_eval []=[]

add_to_env :: EnvEval p -> (Var, Val p) -> EnvEval p
add_to_env vars v = v:vars

lookup_var :: [(Var, Val p)] -> Var -> Val p
lookup_var ((a, b):xs) v = if a==v then b else lookup_var xs v

-- Oblicza wartość wyrażenia
eval_helper :: EnvEval p -> Expr p -> Val p
eval_helper env (ENum p n) = (IVal n)
eval_helper env (EBool p b) = (BVal b)
eval_helper env (EVar p v) = lookup_var env v
eval_helper env (EUnary p op e) =
  let num1 = eval_helper env e
  in
    case num1 of
      ErrVal -> ErrVal
      _ -> case op of
             UNeg -> IVal (-(to_int num1))
             UNot -> BVal (not (to_bool num1))

eval_helper env (EBinary p BDiv e1 e2) =
  let num1 = eval_helper env e1
      num2 = eval_helper env e2
  in
    case num1 of
      IVal i -> case num2 of
                  IVal j -> if j /= 0 then IVal (i `div` j)
                            else ErrVal
                  _ -> ErrVal
      _ -> ErrVal
    
eval_helper env (EBinary p BMod e1 e2) =
  let num1 = eval_helper env e1
      num2 = eval_helper env e2
  in
    case num1 of
      IVal i -> case num2 of
                  IVal j -> if j /= 0 then IVal (i `mod` j)
                            else ErrVal
                  _ -> ErrVal
      _ -> ErrVal

eval_helper env (EBinary p op e1 e2) =
    let num1 = eval_helper env e1
        num2 = eval_helper env e2
    in
      case num1 of
        ErrVal -> ErrVal
        _ -> case num2 of
               ErrVal -> ErrVal
               _ -> case op of
                     BAnd -> BVal ((to_bool num1) && (to_bool num2))
                     BOr  ->  BVal ((to_bool num1) || (to_bool num2))
                     BEq  ->  BVal ((to_int num1) == (to_int num2))
                     BNeq -> BVal ((to_int num1) /= (to_int num2))
                     BGe  ->  BVal ((to_int num1) >= (to_int num2))
                     BLe  ->  BVal ((to_int num1) <= (to_int num2))
                     BLt  ->  BVal ((to_int num1) < (to_int num2))
                     BGt  ->  BVal ((to_int num1) > (to_int num2))
                     BSub -> IVal ((to_int num1) - (to_int num2))
                     BAdd -> IVal ((to_int num1) + (to_int num2))
                     BMul -> IVal ((to_int num1) * (to_int num2))

      
eval_helper env (ELet p variable e1 e2) = 
    eval_helper (add_to_env env (variable, (eval_helper env e1))) e2
      
eval_helper env (EIf p e1 e2 e3) =
  let res = eval_helper env e1 in
    case res of
      ErrVal -> ErrVal
      BVal b -> if b then eval_helper env e2
                else eval_helper env e3
      _ -> ErrVal
    
eval_helper env (EUnit p) = UVal

eval_helper env (EFst p e1) =
  let res = eval_helper env e1
  in case res of
      ErrVal -> ErrVal
      _ -> fst (to_pair res)
        
eval_helper env (ESnd p e1) =
  let res = eval_helper env e1
  in case res of
       ErrVal -> ErrVal
       _ -> snd (to_pair res)
    
eval_helper env (EPair p e1 e2) =
  let res1 = eval_helper env e1
      res2 = eval_helper env e2
  in
    case res1 of
      ErrVal -> ErrVal
      _ -> case res2 of
             ErrVal -> ErrVal
             _ -> PVal (res1, res2)
    
eval_helper env (EApp p e1 e2) =
  let res = eval_helper env e1 
      v = eval_helper env e2
  in
   case res of
     ClosureVal env_fs f -> eval_helper ((funcArg f, v):env) (funcBody f)
     ClosureLambda env_l expr_l var -> eval_helper ((var, v):env_l) expr_l
     _ -> ErrVal

eval_helper env (EFn p var typevar e1) =
   ClosureLambda env e1 var
        
eval_helper env (ENil p t) = LVal []

eval_helper env (ECons p e1 e2) =
  let val1 = eval_helper env e1
      val2 = eval_helper env e2
  in
    case val1 of
      ErrVal -> ErrVal
      _ -> case val2 of
             ErrVal -> ErrVal
             _ -> LVal (val1:(get_list val2))
        
eval_helper env (EMatchL p e1 e2 (x1, x2, e3)) =
    let val1 = eval_helper env e1
    in 
      case val1 of
        LVal [] -> eval_helper env e2
        LVal (x:xs) ->  eval_helper (add_to_env (add_to_env env (x1, x)) (x2, LVal xs)) e3
        _ -> ErrVal


get_list :: Val p -> [Val p]
get_list (LVal v) = v

to_int :: Val p -> Integer
to_int (IVal x) = x

to_bool :: Val p ->  Bool
to_bool (BVal x) = x

to_pair :: Val p -> (Val p, Val p)
to_pair (PVal (x, y)) = (x, y)
