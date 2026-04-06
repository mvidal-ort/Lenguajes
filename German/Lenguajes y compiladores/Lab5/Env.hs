module Env where

import AbsER 
import ErrM 
import qualified Data.Map as Map

import Prelude hiding (fail)
import Control.Monad hiding (fail)
fail = Bad

--Enviroment
type Env = (Alfa, ExpR)
type Alfa = Map.Map AlfaName [Symbol]
type ExpR = Map.Map ExpName (Exp, AlfaName)

lookupAlfabeto :: Env -> AlfaName -> Err [Symbol]
lookupAlfabeto (alfabetoEnv, _) alfaName =
    case Map.lookup alfaName alfabetoEnv of
        Just symbols -> return symbols
        Nothing -> fail $ "No existe el alfabeto: " ++ show alfaName

addAlfabeto :: Env -> AlfaName -> [Symbol] -> Err Env
addAlfabeto (alfabetoEnv, expRegEnv) alfaName symbols =
    if Map.member alfaName alfabetoEnv
        then fail $ "Ya existe el Alfabeto: " ++ show alfaName
        else return (Map.insert alfaName symbols alfabetoEnv, expRegEnv)

lookupExpReg :: Env -> ExpName -> Err (Exp, AlfaName)
lookupExpReg (_, expRegEnv) expName =
    case Map.lookup expName expRegEnv of
        Just expData -> return expData
        Nothing -> fail $ "No existe la Expresión: " ++ show expName

addExpReg :: Env -> ExpName -> Exp -> AlfaName -> Err Env
addExpReg (alfabetoEnv, expRegEnv) expName expr alfaName =
    if Map.member expName expRegEnv
        then fail $ "Ya existe la expresión: " ++ show expName
        else return (alfabetoEnv, Map.insert expName (expr, alfaName) expRegEnv)

emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)