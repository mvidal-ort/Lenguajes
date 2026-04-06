module TypeChecker where

import AbsER
import Env
import ErrM
import qualified Data.Map as Map

import Prelude hiding (fail)
import Control.Monad hiding (fail)
fail = Bad

typecheck :: Prog -> Err Prog
typecheck (Prog alfabetos expRegs) = do
    env <- foldM checkAlfabeto emptyEnv alfabetos
    checkedExpRegs <- mapM (checkExpReg env) expRegs
    return (Prog alfabetos checkedExpRegs)

checkAlfabeto :: Env -> Alfabeto -> Err Env
checkAlfabeto env (Alfabeto alfaName symbols) =
    addAlfabeto env alfaName symbols

checkExpReg :: Env -> ExpReg -> Err ExpReg
checkExpReg env (ExpReg expName expr alfaName) = do
    -- Existe el alfabeto alfaNAme?
    symbols <- lookupAlfabeto env alfaName
    -- expr es valida en AlfaName?
    typedExp <- checkExp env symbols expr alfaName
    return (ExpReg expName typedExp alfaName)

checkExp :: Env -> [Symbol] -> Exp -> AlfaName -> Err Exp
checkExp env symbols expr alfaName = case expr of
    Evacio -> return (ETyped Evacio alfaName)
    EPalabraVacia -> return (ETyped EPalabraVacia alfaName)
    Esymbol symbol ->
        if (elem symbol symbols)
            then return (ETyped (Esymbol symbol) alfaName)
            else Env.fail ("Símbolo no definido en el alfabeto: " ++ show symbol)
    EConcat exp1 exp2 -> do
        typedExp1 <- checkExp env symbols exp1 alfaName
        typedExp2 <- checkExp env symbols exp2 alfaName
        return (ETyped (EConcat typedExp1 typedExp2) alfaName)
    EUnion exp1 exp2 -> do
        typedExp1 <- checkExp env symbols exp1 alfaName
        typedExp2 <- checkExp env symbols exp2 alfaName
        return (ETyped (EUnion typedExp1 typedExp2) alfaName)
    EIter exp1 -> do
        typedExp1 <- checkExp env symbols exp1 alfaName
        return (ETyped (EIter typedExp1) alfaName)