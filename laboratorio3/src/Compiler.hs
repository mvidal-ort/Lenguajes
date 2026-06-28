{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Compiler where

-- java virtual machine instructions reference
-- http://cs.au.dk/~mis/dOvs/jvmspec/ref-Java.html

import AbsCPP
import TypeChecker

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe   
-- import Control.Exception (CompactionFailed)
-- import Control.Monad.Accum (MonadAccum(accum))
-- import GHC.IO.Encoding (CodingProgress(InvalidSequence))
-- import GHC.RTS.Flags (DebugFlags(stm))

instance MonadFail Identity where
  fail = error

type Instruction = String
type FunType     = String
type Label       = Int

data Env = Env {
  funs      :: Map.Map Id FunType,
  vars      :: [Map.Map Id Int],    -- Stack of variables blocks !
  maxvars   :: [Int],               -- Stack of counter for variable addresses in each block
  maxstk    :: Int,                 -- counter for maximal stack depth
  labels    :: Int,                 -- counter for jump labels
  code      :: [Instruction] 
}

emptyEnv :: Env
emptyEnv = Env { funs=Map.empty,
                 vars=[],
                 maxvars=[0],
                 maxstk=0,
                 labels=0,
                 code=[]}

emit :: Instruction -> State Env () -- Con el emit quiero acceder al campo code (instrucciones) de Env 
emit i = do                         --quiero agregar la instrucción que recibo al principio de la lista code
  env <- get                        -- para obtener el env
  put (env {code = i: code env})-- agarro el record env, pero solo quiero modificar el field code, en Haskell, el field es como una funcion por eso hago code env
  -- code env me trae lo que tengo en el field code y como es una lista le pongo el elemento i delante.
-- alternativamente se puede hacer con modify (que hace al tiempo el get y el put)
-- emit :: Instruction -> State Env ()
-- emit i =
--   modify (\env -> env { code = i : code env })

typeSize :: Type -> Int
typeSize Type_double = 2
typeSize _           = 1

-- quiere agregar algo en el field vars de env  
-- tiene relacion con maxvars, ahi se guarda el M proxima posicion de memoria libre.
-- maxvars es una lista porque es un stack de contextos, y requiere un M por contexto
extendVar :: Type -> Id -> State Env () 
extendVar t i = do
  env <- get -- obtiene el env actual
  let m = head (maxvars env) -- uso let para que quede mas claro
  put (env {vars = Map.insert i m (head (vars env)): tail (vars env), 
            maxvars = (m + typeSize t): tail (maxvars env)})
-- tomo el env, accedo al field vars y le agrego la variable i, con posicion de memoria que saco 
-- del tope de la lista maxvars y en el tope de la lista de vars
-- tambien modifico maxvars porque tengo que actualizar el cual sera el valor libre de memoria 
-- ya que inserte una varible
-- tanto vars como maxvars son listas y lo que modifico es el tope, por eso agrego tail 

-- esto es para una declaracion de multiples variables con un solo tipo
-- en el AbsCPP esta esto : SDecls Type [Id], dentro de data stm
extendVars :: Type -> [Id] -> State Env ()
extendVars t ids = mapM_ (\ id -> extendVar t id) ids
-- se puede hacer curryficacion Haskell sabe que si no se pone el lambda, se comporta como una 
-- funcion que espera el siguiente argumento
-- extendVars t ids = mapM (extendVar t) ids
-- de hecho haskell permite que ni siquiera sea necesario el segundo parametro
-- se referencia en la llamada y listo: extendVars t = mapM (extendVar t)


typeJVM :: Type -> String
typeJVM Type_int    = "I"
typeJVM Type_double = "D"
typeJVM Type_void   = "V"
typeJVM Type_bool   = "Z"
typeJVM Type_string = "Ljava/lang/String;"

funJVMType :: String -> [Type] -> Type -> FunType
funJVMType i typs rty = i ++ "(" ++ (foldr (\ t s -> typeJVM t ++ s) "" typs) ++ ")" ++ typeJVM rty

funJVM :: String -> String -> [Type] -> Type -> FunType
funJVM i clas argty rty = "invokestatic " ++ clas ++"/" ++ funJVMType i argty rty

-- algo similar a extendVars, pero en el campo funs. Es mas facil porque es un unico map
extendFunEnv :: String -> FunType -> State Env ()
extendFunEnv fun funtype = modify (\env -> env { funs = Map.insert (Id fun) funtype (funs env)})
-- extendFunEnv :: String -> FunType -> State Env ()
-- extendFunEnv i ft = do
--   env <- get
--   put env { funs = Map.insert (Id i) ft (funs env) }


extendDef :: String -> Def -> State Env ()
extendDef cls (DFun t (Id i) args stmts) =
  extendFunEnv i (funJVM i cls (map (\(ADecl ty _) -> ty) args) t)

-- Besides added in type checker
extendBuiltinDefs :: State Env ()
extendBuiltinDefs = mapM_ ( \ ((Id i),(argTys,rty)) -> extendFunEnv i $ funJVM i "Runtime" argTys rty) buildInFunctions

-- tiene que poner un map vacio en vars, y tocar tambien maxvars
newBlock :: State Env ()
newBlock = modify  (\env -> env { vars = Map.empty: vars env,
                                  maxvars = head (maxvars env): maxvars env })

-- hace pop de vars y maxvars
exitBlock :: State Env ()
exitBlock = modify (\ env -> env { vars = tail (vars env),
                                   maxvars = tail (maxvars env)})

-- genera nuevo label a partir del int que esta en el campo
-- hay que hacerlo string con show y actualizar el campo para el siguiente label
-- 
newLabel :: State Env String
newLabel  = do
  env <- get
  put (env {labels = labels env + 1})
  return ("label" ++ show(labels env))

-- busca en env el campo funs
lookupFun :: Id -> State Env FunType
lookupFun fun = do
  env <- get
  return (fromJust (Map.lookup fun (funs env)))
  -- hago fromjust, porque el lookup devuelve un maybe y necesito solo el just
  -- es seguro hacerlo porque el lookup va a devolver siempre una funcion ya que fue 
  -- verificado antes por typechecker.
  
lookupVar :: Id -> State Env Int
lookupVar i = do
  env <- get
  return (fromJust (lookupVar' i (vars env)))

lookupVar' :: Id -> [Map.Map Id Int] -> Maybe Int
lookupVar' i [] = Nothing
lookupVar' i (m:ms) =
  case Map.lookup i m of
    Just n  -> Just n
    Nothing -> lookupVar' i ms

-- Entry point from ccpp.
-- Arguments: cls is the class name and p is the typed embedded abstract syntax tree (returned by the type checker).
-- Hints: call compileP and run the State monad !
-- esto engancha todo y es como correr la mónada de estado (runstate), espera un ambiente
-- le paso un ambiente vacío para iniciar
-- en realidad el runstate devuelve varias cosas, pero me interesa el estado final por eso se usa execstate
compile :: String -> Program -> [Instruction]
compile cls p = reverse (code (execState (compileP cls p) emptyEnv) )
-- Las instrucciones en emit se insertan al principio, pero quiero que la primera que inserto sea
-- la primera de la lista, por eso el reverse.
-- el exceState me devuelve todo el ambiente, pero solo quiero la lista de instrucciones (code)
-- por eso le hago code al execState
-- compileP es la mónada de estado 


compileP :: String -> Program -> State Env () 
compileP cls (PDefs defs) = do
  emit $ ".class public " ++ cls
  emit $ ".super java/lang/Object"
  emit $ ""
  emit $ ".method public <init>()V"
  emit $ "  aload_0"
  emit $ "  invokenonvirtual java/lang/Object/<init>()V"
  emit $ "  return"
  emit $ ".end method"
  emit $ ""
  extendBuiltinDefs
  mapM  (extendDef cls) defs
  mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun t (Id i) args stmts) = do
  newBlock 
  if i == "main" then do
       emit $ ".method public static main([Ljava/lang/String;)V"
       extendVar Type_string (Id "args") -- in fact is an array of strings ([Ljava/lang/String;)
  else emit $ ".method public static " ++ (funJVMType i (map (\ (ADecl t _) -> t) args) t)
  emit $ ".limit locals 1000"  -- aca se puede ser mas precisoo !
  emit $ ".limit stack  1000"  -- aca se puede ser mas precisoo !
  mapM (\ (ADecl t i) -> extendVar t i) args
  mapM compileStm stmts
  exitBlock
  emit $ "return"
  emit $ ".end method"
  emit ""

compileStm :: Stm -> State Env ()

compileStm (SExp (ETyped exp typ)) = do
  compileExp (ETyped exp typ)
  case typ of
    Type_double -> emit "pop2"
    Type_void   -> return ()
    _           -> emit "pop"

-- NOTA DE DISEÑO SOBRE EL BALANCE DE LA PILA DE OPERANDOS DE LA JVM:
-- En C++, una expresión suelta seguida de un punto y coma es una sentencia válida.
-- Al evaluar dicha expresión mediante 'compileExp', el bytecode generado deja
-- obligatoriamente el resultado de la evaluación en el tope de la pila de la JVM.
-- Dado que el valor de una sentencia suelta no se asigna ni se consume, es crítico
-- limpiar la pila para evitar desbordamientos o fallos del verificador en tiempo
-- de ejecución (java.lang.VerifyError).
--
-- Para ello, se desestructura el nodo 'SExp' abstrayendo directamente la expresión
-- interna sin tipo ('exp') junto con su tipo verificado ('typ'). Dependiendo de este
-- tipo, se emite la instrucción de descarte correspondiente:
--   * 'pop2': Para elementos de tamaño 2 en la JVM (Type_double).
--   * 'return ()': No se altera la pila si la expresión no produce valor (Type_void).
--   * 'pop': Para elementos de tamaño 1 (Type_int, Type_bool y Type_string).
-- ============================================================================


compileStm (SDecls t ids) = extendVars t ids

compileStm (SInit t i exp) = do
  extendVar t i
  compileExp exp
  n <- lookupVar i
  case t of
    Type_double -> emit ("dstore " ++ show n)
    Type_string -> emit ("astore " ++ show n)
    _           -> emit ("istore " ++ show n)

compileStm (SReturn exp) = do
  compileExp exp
  case exp of
    ETyped _ Type_double -> emit "dreturn"
    ETyped _ Type_string -> emit "areturn"
    _                    -> emit "ireturn"

compileStm SReturnVoid = emit "return"

compileStm (SWhile exp stm) = do
  lstart <- newLabel
  lend   <- newLabel
  emit (lstart ++ ":")
  compileExp exp
  emit ("ifeq " ++ lend)
  compileStm stm
  emit ("goto " ++ lstart)
  emit (lend ++ ":")

compileStm (SBlock stms) = do
  newBlock
  mapM_ compileStm stms
  exitBlock

compileStm (SIfElse exp stm1 stm2) = do
  lelse <- newLabel
  lend  <- newLabel
  compileExp exp
  emit ("ifeq " ++ lelse)
  compileStm stm1
  emit ("goto " ++ lend)
  emit (lelse ++ ":")
  compileStm stm2
  emit (lend ++ ":")


-- La opcion escrita respeta los niveles de compilación (program->def->stm->exp)
-- En clase se hizo una version que llama directamente a compile salteandose los exp
-- compileStm (SExp (ETyped e typ)) = do
--   compile (Etype e typ)
--   if (typ `elem` [Type_int, Type_bool, Type_string]) then --son las comillas correctas?
--     emit "pop"
--   else if (typ == Type_double) then
--     emit "pop2"
--   else -- type es void
--     emit "return"
-- compileStm (SDecls ty ids) = extendVars ty ids 
-- compileStm (SWhile exp stm) = do
--   test <- newLabel
--   end <- newLabel
--   emit (test ++ ":")
--   compileExp exp
--   emit ("ifeq" ++ end)
--   compileStm stm
--   emit ("goto" ++  test)
--   emit (end ++ ":")


compileExp :: Exp -> State Env ()
compileExp (ETyped ETrue Type_bool) = emit "ldc 1"
compileExp (ETyped EFalse Type_bool) = emit "ldc 0"
compileExp (ETyped (EInt i) Type_int) = emit ("ldc " ++ show i)
compileExp (ETyped (EDouble d) Type_double) = emit ("ldc2_w " ++ show d)
compileExp (ETyped (EId i) Type_int) = do
  n <- lookupVar i
  emit ("iload " ++ show n)
compileExp (ETyped (EId i) Type_double) = do
  n <- lookupVar i
  emit ("dload " ++ show n)
compileExp (ETyped (EId i) Type_bool) = do
  n <- lookupVar i
  emit ("iload " ++ show n)
compileExp (ETyped (EId i) Type_string) = do
  n <- lookupVar i
  emit ("aload " ++ show n)
compileExp (ETyped (EApp f exps) _) = do
  mapM_ compileExp exps
  call <- lookupFun f
  emit call
compileExp (ETyped (EAss i e) Type_int) = do
  compileExp e
  n <- lookupVar i
  emit ("istore " ++ show n)
  emit ("iload " ++ show n)
compileExp (ETyped (EAss i e) Type_double) = do
  compileExp e
  n <- lookupVar i
  emit ("dstore " ++ show n)
  emit ("dload " ++ show n)
compileExp (ETyped (EAss i e) Type_bool) = do
  compileExp e
  n <- lookupVar i
  emit ("istore " ++ show n)
  emit ("iload " ++ show n)
compileExp (ETyped (EAss i e) Type_string) = do
  compileExp e
  n <- lookupVar i
  emit ("astore " ++ show n)
  emit ("aload " ++ show n)


compileExp (ETyped (EIncr i) Type_int) = do
  n <- lookupVar i
  emit ("iinc " ++ show n ++ " 1")
  emit ("iload " ++ show n)
compileExp (ETyped (EDecr i) Type_int) = do
  n <- lookupVar i
  emit ("iinc " ++ show n ++ " -1")
  emit ("iload " ++ show n)
compileExp (ETyped (EPIncr i) Type_int) = do
  n <- lookupVar i
  emit ("iload " ++ show n)
  emit ("iinc " ++ show n ++ " 1")
compileExp (ETyped (EPDecr i) Type_int) = do
  n <- lookupVar i
  emit ("iload " ++ show n)
  emit ("iinc " ++ show n ++ " -1")
compileExp (ETyped (EDecr i) Type_double) = do
  n <- lookupVar i
  emit ("dload " ++ show n)
  emit "ldc2_w 1.0"
  emit "dsub"
  emit ("dstore " ++ show n)
  emit ("dload " ++ show n)
compileExp (ETyped (EPIncr i) Type_double) = do
  n <- lookupVar i
  emit ("dload " ++ show n)
  emit ("dload " ++ show n)
  emit "ldc2_w 1.0"
  emit "dadd"
  emit ("dstore " ++ show n)
compileExp (ETyped (EPDecr i) Type_double) = do
  n <- lookupVar i
  emit ("dload " ++ show n)
  emit ("dload " ++ show n)
  emit "ldc2_w 1.0"
  emit "dsub"
  emit ("dstore " ++ show n)
compileExp (ETyped (EString s) Type_string) = emit ("ldc " ++ show s)

compileExp (ETyped (EPlus a b) typ) = do  
  compileExp a
  compileExp b
  case typ of
    Type_int -> emit "iadd"
    Type_double -> emit "dadd"
    Type_string -> do
      appFunConcat <- lookupFun (Id "concatStr")
      emit appFunConcat
compileExp (ETyped (EMinus a b) typ) = do
  compileExp a
  compileExp b
  case typ of
    Type_int    -> emit "isub"
    Type_double -> emit "dsub"
compileExp (ETyped (ETimes a b) typ) = do
  compileExp a
  compileExp b
  case typ of
    Type_int    -> emit "imul"
    Type_double -> emit "dmul"
compileExp (ETyped (EDiv a b) typ) = do
  compileExp a
  compileExp b
  case typ of
    Type_int    -> emit "idiv"
    Type_double -> emit "ddiv"


-- Menor que
compileExp (ETyped (ELt a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b Lt
compileExp (ETyped (ELt a b) Type_bool) = compileComparatorInt a b Lt
-- Mayor que
compileExp (ETyped (EGt a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b Gt
compileExp (ETyped (EGt a b) Type_bool) = compileComparatorInt a b Gt
-- Menor o igual 
compileExp (ETyped (ELtEq a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b Le
compileExp (ETyped (ELtEq a b) Type_bool) = compileComparatorInt a b Le
-- Mayor o igual 
compileExp (ETyped (EGtEq a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b Ge
compileExp (ETyped (EGtEq a b) Type_bool) = compileComparatorInt a b Ge
-- Igual
compileExp (ETyped (EEq a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b Equal
compileExp (ETyped (EEq a b) Type_bool) = compileComparatorInt a b Equal
-- Distinto 
compileExp (ETyped (ENEq a@(ETyped _ Type_double) b) Type_bool) = compileComparatorDouble a b NEqual
compileExp (ETyped (ENEq a b) Type_bool) = compileComparatorInt a b NEqual

-- En estos casos de comparacion; compileExp (ETyped (ELt a b) Type_double) no tiene sentido 
-- porque nunca va a ser un Type_double. Ese type hace referencia al tipo del resultado de la comparación. 
-- Siempre es bool, y lo que hay que hacer es preguntar por los tipos de los operandos. 
-- Basta que sea solo el primero porque el typechecker ya garantiza que son el mismo tipo. 
-- Ademas, solo es necesario distinguir el tipo double del operando y usar compileComparatorDouble; 
-- si no es double usa la otra regla con compileComparatoInt, que es válido tanto para int como para bool 

compileExp (ETyped (EIncr i) Type_double) = do
  n <- lookupVar i
  emit ("dload " ++ show n)
  emit "ldc2_w 1.0"
  emit "dadd"
  emit ("dstore " ++ show n)
  emit ("dload " ++ show n)
compileExp (ETyped (EAnd exp1 exp2) Type_bool) = do
  lfalse <- newLabel
  lend   <- newLabel
  compileExp exp1
  emit ("ifeq " ++ lfalse)
  compileExp exp2
  emit ("ifeq " ++ lfalse)
  emit "ldc 1"
  emit ("goto " ++ lend)
  emit (lfalse ++ ":")
  emit "ldc 0"
  emit (lend ++ ":")
compileExp (ETyped (EOr exp1 exp2) Type_bool) = do
  ltrue <- newLabel
  lend  <- newLabel
  compileExp exp1
  emit ("ifne " ++ ltrue)
  compileExp exp2
  emit ("ifne " ++ ltrue)
  emit "ldc 0"
  emit ("goto " ++ lend)
  emit (ltrue ++ ":")
  emit "ldc 1"
  emit (lend ++ ":")


compileComparatorInt :: Exp -> Exp -> Cmp -> State Env ()
compileComparatorInt exp1 exp2 cmp = do
  compileExp exp1
  compileExp exp2

  ltrue <- newLabel
  lend  <- newLabel

  emit (show cmp ++ ltrue)
  emit "ldc 0"
  emit ("goto " ++ lend)
  emit (ltrue ++ ":")
  emit "ldc 1"
  emit (lend ++ ":")

compileComparatorDouble :: Exp -> Exp -> Cmp -> State Env ()
compileComparatorDouble exp1 exp2 cmp = do
  compileExp exp1
  compileExp exp2

  ltrue <- newLabel
  lend  <- newLabel

  emit "dcmpg"
  emit (showDbl cmp ++ ltrue)
  emit "ldc 0"
  emit ("goto " ++ lend)
  emit (ltrue ++ ":")
  emit "ldc 1"
  emit (lend ++ ":")

-- Hints: usefull auxiliary functions for comparations compilation
data Cmp = Equal | NEqual | Lt | Gt | Ge | Le
  deriving (Eq)

instance Show Cmp where
  show Equal  = "if_icmpeq "
  show NEqual = "if_icmpne "
  show Lt     = "if_icmplt "
  show Gt     = "if_icmpgt "
  show Ge     = "if_icmpge "
  show Le     = "if_icmple "

showDbl :: Cmp -> Instruction
showDbl Equal  = "ifeq "
showDbl NEqual = "ifne "
showDbl Lt     = "iflt "
showDbl Gt     = "ifgt "
showDbl Ge     = "ifge "
showDbl Le     = "ifle "

  



