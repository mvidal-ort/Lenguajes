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

-- Verifica las funciones: data Def = DFun Type Id [Arg] [Stm]
-- Verificar: parámetros válidos, variables bien declaradas, expresiones bien tipadas, returns correctos
checkDef :: Env -> Def -> Err ()
checkDef env (DFun t _ args stms) = do -- t = tipo de retorno, _ = nombre de función, args = parámetros, stms = cuerpo
  env'  <- addArgs (newBlock env) args -- en un scope vacio, carga los argumentos de la funcion, creando un contexto local
  _     <- checkStms t env' stms -- Verifica los stms, Si falla se cancela toda la ejecucion, si no falla no me importa, por eso lo dejo con _
  return ()

-- Auxiliar. Para cargar los argumentos de la función.
addArgs :: Env -> [Arg] -> Err Env
addArgs env [] = return env
addArgs env (ADecl t id : args) = do
  env' <- updateVar env id t
  addArgs env' args


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


inferExp :: Env -> Exp -> Err Type
inferExp _ (EInt _) = return Type_int
inferExp _ ETrue = return Type_bool
inferExp _ EFalse = return Type_bool
inferExp _ (EDouble _) = return Type_double
inferExp _ (EString _) = return Type_string
inferExp env (EId x) = lookupVar env x

inferExp _ _ = Bad "expresion no implementada"


-- data Exp
    
--     | EApp Id [Exp]
--     | EPIncr Exp
--     | EPDecr Exp
--     | EIncr Exp
--     | EDecr Exp
--     | ETimes Exp Exp
--     | EDiv Exp Exp
--     | EPlus Exp Exp
--     | EMinus Exp Exp
--     | ELt Exp Exp
--     | EGt Exp Exp
--     | ELtEq Exp Exp
--     | EGtEq Exp Exp
--     | EEq Exp Exp
--     | ENEq Exp Exp
--     | EAnd Exp Exp
--     | EOr Exp Exp
--     | EAss Exp Exp
--     | ETyped Exp Type






checkExp :: Env -> Exp -> Type -> Err ()
checkExp env e expected = do
  t <- inferExp env e
  if t == expected
    then return ()
    else Bad "Type mismatch"

-- En clase, la idea es que checkstms hace el checkeo para todos los statemens, entonces cada tipo de stm se hace como regla del checkstms
-- La otra version hace un loop dentro de checkstms, y en cada loop llama a a una auxiliar checkstm (sin s) y procesa cada statement intependiente
-- en la checkStm :: Type -> Env -> Stm -> Err Env, haciendo un case por cada definicion de statement (SExp,SDecls, etc)
-- ver imagenes lenguajesLabo2_...faltan las otras stm
-- En lenguajesLabo2_5-Sdecls faltan las otras stm, esta mal, desagrgó la lista para explicar la idea
-- tal como en la otra version , para hacer esto recoemienda usar foldm para hacer la recursion para actualizar el env. similar a lo hech9o en checkDef
-- foldm calcula un acumulador en cada pasa y lo inyecta en el paso siguiente, es asi como el env se va completando, iterarcion a iteracion


checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms _ env [] = return env
checkStms retType env (s:ss) =
  case s of
    SExp e -> do
      _ <- inferExp env e
      checkStms retType env ss
    SDecls t ids -> do
      env' <- addVars env t ids
      checkStms retType env' ss
    SInit t x e -> do
      checkExp env e t
      env' <- updateVar env x t
      checkStms retType env' ss
    SReturn e -> do
      checkExp env e retType
      checkStms retType env ss
    SReturnVoid ->
      if retType == Type_void
        then checkStms retType env ss
        else Bad "Non-void function must return a value"
    SBlock blockStms -> do
      _ <- checkStms retType (newBlock env) blockStms
      checkStms retType env ss
    SIfElse cond s1 s2 -> do
      checkExp env cond Type_bool
      _ <- checkStms retType env [s1]
      _ <- checkStms retType env [s2]
      checkStms retType env ss
    SWhile cond body -> do
      checkExp env cond Type_bool
      _ <- checkStms retType env [body]
      checkStms retType env ss

-- Auxiliar, agrega variables al entorno (SDecls de checkStms)
addVars :: Env -> Type -> [Id] -> Err Env
addVars env _ [] = return env
addVars env t (x:xs) = do
  env' <- updateVar env x t
  addVars env' t xs

