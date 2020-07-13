-- Pracownia 5, Metody Programowania.
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
typecheck fs vars expr = case (infer_type  (create_function_map fs) "" (create_env_typecheck vars) expr) of
                            Right TInt -> Ok
                            Right t -> Error (getData expr) "wrong result type"
                            Left (Err p m) -> Error p m


create_function_map :: [FunctionDef p] -> [(FSym, (FunctionDef p))]
create_function_map (x:xs) = (funcName x, x) : create_function_map xs

create_env_typecheck :: [Var] -> Env
create_env_typecheck (x:xs) = (x, TInt) : create_env_typecheck xs
create_env_typecheck [] = []

-- sprawdza czy zmienna jest w środowisku
is_in_env :: Env -> Var -> Bool
is_in_env ((a, b):xs) v = if a==v then True else is_in_env xs v
is_in_env [] _ = False

lookup_function :: [(FSym, (FunctionDef p))] -> FSym -> FunctionDef p
lookup_function ((fname, def):xs) name | fname==name = def
                                       | otherwise = lookup_function xs name

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

infer_type :: [(FSym, (FunctionDef p))]-> FSym -> Env -> Expr p  -> Either (Error p) Type
infer_type fs caller env (EVar p v) =
    case (lookup_type env v) of
        Left False -> Left (Err p "undefined variable")
        Right t -> Right t

-- Argument caller zawiera nazwę funkcji wywołującej, może być pusty. 
-- Dzięki temu infer_type nie zapętli się przy
-- wyprowadzaniu typu funkcji rekurencyjnej.            
infer_type fs caller env (ENum p n) = Right TInt
infer_type fs caller env (EBool p n) = Right TBool
infer_type fs caller env (EUnary p op e) =
    case op of
        UNot -> case (infer_type fs caller env e) of
                    Right TBool -> Right TBool
                    Right t -> Left (Err p "wrong type")
                    Left err -> Left err
        UNeg -> case (infer_type fs caller env e) of
                    Right TInt -> Right TInt
                    Right t -> Left (Err p "wrong type")
                    Left err ->Left err


infer_type fs caller env (EBinary p op e1 e2) =
    let typ1 = (infer_type fs caller env e1)
        typ2 = (infer_type fs caller env e2)
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
            
infer_type fs caller env (ELet p variable e1 e2) =
    case (infer_type fs caller env e1) of
        Right t -> infer_type fs caller (extend_env env (variable, t)) e2
        Left err -> Left (Err p "type error")

infer_type fs caller env (EIf p e1 e2 e3) =
     let type1 = (infer_type fs caller env e1)
         type2 = (infer_type fs caller env e2)
         type3 = (infer_type fs caller env e3)
     in 
        case type1 of
            Right TBool -> case type2 of
                            Right t -> case type3 of
                                         Right t2 -> if t==t2 then Right t else Left (Err p "error type")
                                         Left err -> Left err
                            Left err -> Left err
            Right _ -> Left (Err p "type error")
            Left err -> Left err
                                         
infer_type fs caller env (EApp p name e1) =
    if caller == "" || caller /= name then
        let fun = lookup_function fs name
            argtype = funcArgType fun
            restype = funcResType fun
            exprtype = infer_type fs name env e1
            actual_restype = infer_type fs name [(funcArg fun, argtype)] (funcBody fun)
        in 
           case exprtype of
             Right texpr -> case actual_restype of
                             Right tres -> if (texpr == argtype && tres == restype) then Right tres
                                           else Left (Err p "type error")
                             Left err -> Left err
             Left err -> Left err
    else 
        let fun = lookup_function fs name
            argtype = funcArgType fun
            restype = funcResType fun
            exprtype = infer_type fs caller env e1
        in
            case exprtype of
                Right t -> if t==argtype then Right restype
                           else Left (Err p "type error")
                Left err -> Left err

infer_type fs caller env (EUnit p) = Right TUnit
infer_type fs caller env (EPair p e1 e2) =
    case (infer_type fs caller env e1) of
        Right t1 -> case (infer_type fs caller env e2) of
                        Right t2 -> Right (TPair t1 t2)
                        Left err -> Left err
        Left err -> Left err
infer_type fs caller env (EFst p e1) =
    case (infer_type fs caller env e1) of
        Right (TPair t1 t2) -> Right t1
        Left err -> Left err
infer_type fs caller env (ESnd p e1) =
    case (infer_type fs caller env e1) of
        Right (TPair t1 t2) -> Right t2
        Left err -> Left err
infer_type fs caller env (ENil p t) = Right t
infer_type fs caller env (ECons p e1 e2) =
    case (infer_type fs caller env e1) of
        Right t -> case (infer_type fs caller env e2) of
                       Right (TList t2) -> if t==t2 then Right (TList t2)
                                           else Left (Err p "type error")
                       Left err -> Left err
        Left err -> Left err
        
infer_type fs caller env (EMatchL p expr nclause (x1, x2, e2)) =
    let typelist = infer_type fs caller env expr
        typenclause = infer_type fs caller env nclause
        cexprtype = infer_type fs caller env e2
    in
      case typelist of
        Right (TList t) -> case (infer_type fs caller (extend_env (extend_env env (x1, t)) (x2, (TList t))) e2) of
                             Right typ -> case typenclause of
                                            Right typ2 -> if typ==typ2 then Right typ
                                                          else Left (Err p "type error")
                                            Left err -> Left err
                             Left err -> Left err
        Right _ -> Left (Err p "type error")
        Left err -> Left err

-- Zwraca typ zmiennej v
lookup_type :: Env -> Var -> Either (Bool) Type
lookup_type ((a, b):xs) v = if a==v then (Right b) else lookup_type xs v
lookup_type [] _ = Left False

eval :: [FunctionDef p] -> [(Var, Integer)] -> Expr p -> EvalResult
eval fs vars e = 
  let res = eval_helper (create_function_map fs) (create_env_eval vars) e
  in 
    case res of
      IVal i -> Value i
      BVal _ -> RuntimeError
      UVal -> RuntimeError
      PVal _ -> RuntimeError
      LVal _ -> RuntimeError
      ErrVal -> RuntimeError

data Val = IVal Integer | BVal Bool | UVal | PVal (Val, Val) | LVal [Val] | ErrVal deriving (Eq, Show)

-- Środowisko dla funkcji eval
type EnvEval = [(Var, Val)]

create_env_eval :: [(Var, Integer)] -> EnvEval
create_env_eval ((v, val):vars) = (v, IVal val) : (create_env_eval vars)
create_env_eval []=[]

-- Dodaje zmienną do środowiska
add_to_env :: EnvEval -> (Var, Val) -> EnvEval
add_to_env vars v = v:vars

lookup_var :: [(Var, Val)] -> Var -> Val
lookup_var ((a, b):xs) v = if a==v then b else lookup_var xs v

-- Oblicza wartość wyrażenia
eval_helper :: [(FSym, (FunctionDef p))] -> EnvEval -> Expr p -> Val
eval_helper fs env (ENum p n) = (IVal n)
eval_helper fs env (EBool p b) = (BVal b)
eval_helper fs env (EVar p v) = lookup_var env v
eval_helper fs env (EUnary p op e) =
    let num1 = eval_helper fs env e
    in
        case op of
            UNeg -> (IVal (-(to_int num1)))
            UNot -> (BVal ( not (to_bool num1)))

eval_helper fs env (EBinary p BDiv e1 e2) =
  let num1 = eval_helper fs env e1
      num2 = eval_helper fs env e2
  in
     if (to_int num2) /= 0 then (IVal ((to_int num1) `div` (to_int num2)))
     else ErrVal

eval_helper fs env (EBinary p op e1 e2) =
    let num1 = eval_helper fs env e1
        num2 = eval_helper fs env e2
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
      
eval_helper fs env (ELet p variable e1 e2) = 
    eval_helper fs (add_to_env env (variable, (eval_helper fs env e1))) e2
      
eval_helper fs env (EIf p e1 e2 e3) =
    if (to_bool (eval_helper fs env e1)) then eval_helper fs env e2
    else eval_helper fs env e3
    
eval_helper fs env (EUnit p) = UVal

eval_helper fs env (EFst p e1) =
    fst (to_pair (eval_helper fs env e1))
    
eval_helper fs env (ESnd p e1) =
    snd (to_pair (eval_helper fs env e1))
    
eval_helper fs env (EPair p e1 e2) =
    PVal ((eval_helper fs env e1), (eval_helper fs env e2))
    
eval_helper fs env (EApp p name e1) =
    let fun = lookup_function fs name
        val1 = eval_helper fs env e1
    in 
        eval_helper fs (add_to_env env (funcArg fun, val1)) (funcBody fun)
        
eval_helper fs env (ENil p t) = LVal []

eval_helper fs env (ECons p e1 e2) =
    let val1 = eval_helper fs env e1
        val2 = eval_helper fs env e2
    in
        LVal (val1:(get_list val2))
eval_helper fs env (EMatchL p e1 e2 (x1, x2, e3)) =
    let val1 = eval_helper fs env e1
    in 
      case val1 of
        LVal [] -> eval_helper fs env e2
        LVal (x:xs) ->  eval_helper fs (add_to_env (add_to_env env (x1, x)) (x2, LVal xs)) e3


get_list :: Val -> [Val]
get_list (LVal v) = v

to_int :: Val -> Integer
to_int (IVal x) = x

to_bool :: Val ->  Bool
to_bool (BVal x) = x

to_pair :: Val -> (Val, Val)
to_pair (PVal (x, y)) = (x, y)
