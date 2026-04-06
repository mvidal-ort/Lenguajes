--Germán Gómez
--319482

{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

import Data.Map as Map
import Control.Monad(foldM)

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = do
    env <- recolectarFunciones p
    checkProg env p

recolectarFunciones :: Program -> Err Env
recolectarFunciones(PDefs defs) = 
    foldM(\ env (DFun tipoRetorno fName args stms) -> updateFun env fName (extraerTipos args, tipoRetorno)) emptyEnv defs
    where
        extraerTipos :: [Arg] -> [Type]
        extraerTipos [] = []
        extraerTipos ((ADecl tipo id):args) = tipo : extraerTipos args

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs defs) = mapM_ (checkDef env) defs

checkDef :: Env -> Def -> Err ()
checkDef env (DFun tipoRetorno fName args stms) = do
    --le agrego un context vacio a mi env
    let nuevoEnv = newBlock env
    --Extiendo 
    envActualizado <- foldM (\e (ADecl tipo varName) -> updateVar e varName tipo) nuevoEnv args
    --chequeo duplicados en Args
    tieneDuplicados args
    --checqueo stms
    checkStms tipoRetorno envActualizado stms
    return ()

tieneDuplicados :: Eq a => [a] -> Err ()
tieneDuplicados [] = Ok () 
tieneDuplicados (x:xs)
    | x `elem` xs = Bad "Parámetros duplicados en la función" 
    | otherwise   = tieneDuplicados xs  


inferExp :: Env -> Exp -> Err Type
inferExp env expr = case expr of
    ETrue -> return Type_bool
    EFalse -> return Type_bool
    EInt _ -> return Type_int
    EDouble _ -> return Type_double
    EString _ -> return Type_string
    EId id -> lookupVar env id
    --EApp id args -> return Type_bool --Prueba
    EApp id args -> do
        (paramTypes, tipoRetorno) <- lookupFun env id
        argTypes <- mapM (inferExp env) args
        if paramTypes == argTypes then 
            return tipoRetorno
        else 
            fail $  "El operador '++' solo se puede aplicar a int o double"

    EPIncr exp -> do
        ty <- inferExp env exp
        if (ty == Type_int || ty == Type_double) then 
            return ty
        else 
            fail $  "El operador '++' solo se puede aplicar a int o double"
    EPDecr exp -> do
        ty <- inferExp env exp
        if (ty == Type_int || ty == Type_double) then 
            return ty
        else 
            fail $  "El operador '--' solo se puede aplicar a int o double"
    EIncr exp -> do
        ty <- inferExp env exp
        if (ty == Type_int || ty == Type_double) then 
            return ty
            else 
                fail $  "El operador '++' solo se puede aplicar a int o double"
    EDecr exp -> do
        ty <- inferExp env exp 
        if (ty == Type_int || ty == Type_double) then
            return ty
            else 
                fail $  "El operador '--' solo se puede aplicar a int o double"
    ETimes exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_int
            (Type_double, Type_double) -> return Type_double
            _ -> fail $ "Tipos incorrectos para el operador Times"
    EDiv exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_int
            (Type_double, Type_double) -> return Type_double
            _ -> fail $ "Tipos incorrectos para el operador div"
    EMinus exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_int
            (Type_double, Type_double) -> return Type_double
            _ -> fail $ "Tipos incorrectos para el operador Minus"
    EPlus exp1 exp2 -> do
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_int
            (Type_double, Type_double) -> return Type_double
            (Type_string, Type_string) -> return Type_string
            _ -> fail $ "Tipos incorrectos para el operador plus"
    ELt exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador <"
    EGt exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador >"
    ELtEq exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador <="
    EGtEq exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador >="
    EAnd exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_bool, Type_bool) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador &&"
    EOr exp1 exp2 -> do 
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_bool, Type_bool) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador ||"
    EEq exp1 exp2 -> do
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            (Type_string, Type_bool) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador =="
    ENEq exp1 exp2 -> do
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        case (ty1, ty2) of
            (Type_int, Type_int) -> return Type_bool
            (Type_double, Type_double) -> return Type_bool
            (Type_string, Type_bool) -> return Type_bool
            _ -> fail $ "Tipos incorrectos para el operador !="
    EAss exp1 exp2 -> do
        ty1 <- inferExp env exp1
        ty2 <- inferExp env exp2
        if (ty1 == ty2) then 
            return ty1
         else 
            fail $  "Tipo de asignación incorrecto"
    ETyped exp ty -> do
        tyInfer <- inferExp env exp
        if (tyInfer == ty) then
            return ty
        else 
            fail $  "Tipos no coinciden" 


--inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
--inferBin types env exp1 exp2 = do
--    typ <- inferExp env exp1
--    if (elem typ types) then
--        checkExp env exp2 typ
--    else
--       fail $ "Wrong type of expression " ++ printTree exp1

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp typ = do
    typ2 <- (inferExp env exp)
    if (typ2 == typ) then
        return ()
    else
        fail $ "1 type of " ++ printTree exp ++
               "expected " ++ printTree typ ++
               "but found" ++ printTree typ2

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms tipoRetorno env [] = Ok env
checkStms tipoRetorno env (stm:stms) = do
    env' <- checkStm tipoRetorno env stm
    checkStms tipoRetorno env' stms


checkStm :: Type -> Env -> Stm -> Err Env
checkStm tipoRetorno env stm = case stm of
    SExp expr -> do
        _ <- inferExp env expr
        Ok env
    SDecls ty ids -> do
        foldM (\nuevoEnv id -> updateVar nuevoEnv id ty) env ids

    SInit ty id expr -> do
        exprType <- inferExp env expr
        if ty == exprType
            then updateVar env id ty
            else Bad $ "Tipo de inicialización incorrecto"
    SReturn expr -> do
        exprType <- inferExp env expr
        if exprType == tipoRetorno
            then Ok env
            else Bad "Tipo de retorno no coincide con el tipo declarado de la función"
    SReturnVoid -> 
        if tipoRetorno == Type_void
            then Ok env
            else Bad "Retorno void, pero función no retorna void"

    SWhile exp stm -> do
        checkExp env exp Type_bool 
        checkStm tipoRetorno env stm
    SBlock stms -> do
        let envWithBlock = newBlock env
        checkedEnv <- checkStms tipoRetorno envWithBlock stms
        Ok checkedEnv
    SIfElse cond thenStm elseStm -> do
        condType <- inferExp env cond
        if condType /= Type_bool
            then Bad $ "Condición del if no es bool"
            else do
                let envThen = newBlock env
                _ <- checkStms tipoRetorno envThen [thenStm]
                let envElse = newBlock env
                _ <- checkStms tipoRetorno envElse [elseStm]
                Ok env

