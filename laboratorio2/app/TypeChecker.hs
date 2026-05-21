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
inferExp env (EPlus e1 e2) = inferArith env e1 e2 [Type_int, Type_double, Type_string]
inferExp env (EMinus e1 e2) = inferArith env e1 e2 [Type_int, Type_double]
inferExp env (ETimes e1 e2) = inferArith env e1 e2 [Type_int, Type_double]
inferExp env (EDiv e1 e2) = inferArith env e1 e2 [Type_int, Type_double]
inferExp env (ELt e1 e2) = inferOrdering env e1 e2
inferExp env (EGt e1 e2) = inferOrdering env e1 e2
inferExp env (ELtEq e1 e2) = inferOrdering env e1 e2
inferExp env (EGtEq e1 e2) = inferOrdering env e1 e2
inferExp env (EEq e1 e2) = inferEquality env e1 e2
inferExp env (ENEq e1 e2) = inferEquality env e1 e2
inferExp env (EAnd e1 e2) = inferBoolean env e1 e2
inferExp env (EOr e1 e2) = inferBoolean env e1 e2
inferExp env (EAss lhs rhs) = inferAssign env lhs rhs
inferExp env (EApp f exps) = do
  (argTypes, retType) <- lookupFun env f
  checkArgs env argTypes exps
  return retType
inferExp env (EPIncr e) = inferIncDec env e
inferExp env (EPDecr e) = inferIncDec env e
inferExp env (EIncr e) = inferIncDec env e
inferExp env (EDecr e) = inferIncDec env e
inferExp env (ETyped e t) = do
  t' <- inferExp env e
  if compatible t t'
    then return t
    else Bad "Typed expression mismatch"

inferExp _ _ = Bad "Expresion not implemented"

-- AUXILIAR: Para inferir los tipos de los operadores aritméticos
-- inferArith :: Env -> Exp -> Exp -> [Type] -> Err Type
-- inferArith env e1 e2 validTypes = do
--   t1 <- inferExp env e1
--   t2 <- inferExp env e2
--   if elem t1 validTypes && elem t2 validTypes
--     then return (max t1 t2)
--     else Bad "Arithmetic type error"

-- modificado para que acepte parametros aritmeticos del mismo tipo
inferArith :: Env -> Exp -> Exp -> [Type] -> Err Type
inferArith env e1 e2 validTypes = do
  t1 <- inferExp env e1
  t2 <- inferExp env e2
  if elem t1 validTypes &&
     elem t2 validTypes &&
     t1 == t2
    then return t1
    else Bad "Arithmetic type error"

-- AUXILIARES: Para inferir tipos de los operadores de comparación (<, >, <=, >=, ==, !=)
isNumeric :: Type -> Bool
isNumeric Type_int = True
isNumeric Type_double = True
isNumeric _ = False

-- inferOrdering :: Env -> Exp -> Exp -> Err Type
-- inferOrdering env e1 e2 = do
--   t1 <- inferExp env e1
--   t2 <- inferExp env e2
--   if isNumeric t1 && isNumeric t2
--     then return Type_bool
--     else Bad "Ordering type error"

-- Modificado para que solo acepte paramteros del mismo tipo en las comparaciones
inferOrdering :: Env -> Exp -> Exp -> Err Type
inferOrdering env e1 e2 = do
  t1 <- inferExp env e1
  t2 <- inferExp env e2
  if isNumeric t1 && isNumeric t2 && t1 == t2
    then return Type_bool
    else Bad "Ordering type error"

isEqualityType :: Type -> Bool
isEqualityType Type_bool = True
isEqualityType Type_int = True
isEqualityType Type_double = True
isEqualityType _ = False

-- compatible :: Type -> Type -> Bool
-- compatible t1 t2 =  t1 == t2 || (isNumeric t1 && isNumeric t2)

-- compatible :: Type -> Type -> Bool
-- compatible lhs rhs =
--   lhs == rhs ||
--   (lhs == Type_double && rhs == Type_int)

-- Modificado para que solo acepte parametros del mismo tipo en la asignación
compatible :: Type -> Type -> Bool
compatible t1 t2 = t1 == t2

inferEquality :: Env -> Exp -> Exp -> Err Type
inferEquality env e1 e2 = do
  t1 <- inferExp env e1
  t2 <- inferExp env e2
  if isEqualityType t1 &&
     isEqualityType t2 &&
     compatible t1 t2
    then return Type_bool
    else Bad "Equality type error"

-- AUXILIAR: para inferir tipos en operaciones boolenas (AND, OR)
inferBoolean :: Env -> Exp -> Exp -> Err Type
inferBoolean env e1 e2 = do
  t1 <- inferExp env e1
  t2 <- inferExp env e2
  if t1 == Type_bool &&
     t2 == Type_bool
    then return Type_bool
    else Bad "Boolean type error"

-- AUXILIAR: para inferir tipos en la asignación
inferAssign :: Env -> Exp -> Exp -> Err Type
inferAssign env (EId x) rhs = do
  t1 <- lookupVar env x
  t2 <- inferExp env rhs
  if compatible t1 t2
    then return t1
    else Bad "Assignment type error"
inferAssign _ _ _ =
  Bad "Left side of assignment must be a variable"

-- AUXILIARES: Para llamadas a Funcion EAPP
checkArgs :: Env -> [Type] -> [Exp] -> Err ()
checkArgs env expectedTypes exps = do
  actualTypes <- mapM (inferExp env) exps
  if length expectedTypes /= length actualTypes
    then Bad "Wrong number of arguments"
  else if and (zipWith compatible expectedTypes actualTypes)
    then return ()
  else
    Bad "Argument type error"

-- AUXILIAR: Para inferir tipos en Incremento/Decrementos  x++, ++x, x--, --x
inferIncDec :: Env -> Exp -> Err Type
inferIncDec env (EId x) = do
  t <- lookupVar env x
  if isNumeric t
    then return t
    else Bad "Increment/decrement requires numeric variable"
inferIncDec _ _ = Bad "Increment/decrement requires variable"

----------------------------

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

