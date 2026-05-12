{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck (PDefs defs) = do
  env <- buildSig emptyEnv defs
  checkProg env (PDefs defs)

---- Construye el env, extrayendo los tipos de las funciones declaradas en el programa
---- Se llama a esta funcion desde el arranque con typecheck, pasando un env vacío.
buildSig :: Env -> [Def] -> Err Env
buildSig env [] = return env
buildSig env (d:ds) =
  case d of
    DFun t id args _ -> do --si la definicion es una función, extrae el tipo y lo agrega al env      
      let argTypes = [ ty | ADecl ty _ <- args ]
      env' <- updateFun env id (argTypes, t)
      buildSig env' ds

checkProg :: Env -> Program -> Err ()
checkProg env (PDefs defs) =
  mapM_ (checkDef env) defs

-- Para probar
checkDef :: Env -> Def -> Err ()
checkDef _ _ = return ()

-- Supende temporal para poder probar y que no explote por undefined
-- checkDef :: Env -> Def -> Err ()
-- checkDef = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms = undefined
