{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif


-- Se puede probar con cabal run ...path

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
  mapM_ (checkDef env) defs --curryfica, creando una "nueva funcion" que chequea una definición usando ese env
  -- aplicar checkDef env, a cada definición en secuencia, propagando errores e ignorando resultados 
  --(el guion difiere de mapM, justamente para no quedarme con los resultados)

-- Para probar
checkDef :: Env -> Def -> Err ()
checkDef _ _ = return ()

-- Supende temporal para poder probar y que no explote por undefined
-- checkDef :: Env -> Def -> Err ()
-- checkDef = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined
-- En Eplus, la otra version lo hace con case en los tipos de los sumandos. (asi lo hace en el libro)
-- En clase hace un elem del tipo para ver si esta en la lista de posibles tipos de sumandos [int, double, string]
-- Como data Type tiene deriving Ord, puede comparalos y hace un max de los tipos inferidos para devolver el tipo de la suma
-- asume el orden de la efinicion bool < int < double < string
-- ver captura labo2-tipoSuma
-- el libro recomienda la primera opcion porque el Eminus es el mismo codigo pero no se permite string, entonces tiene mas sentido
-- hacer el case.
-- el libro hace una abstraccion para operadores binarios y junta todo el codigo en un solo checkeo, ver labo2-tipoBinario

-- tipo de variable Eid
-- no se precisa el DO, se puede llamar directamente al lookup, pero es lo mismo
-- ver labo2-tipoVariable

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms = undefined
