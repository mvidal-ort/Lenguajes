{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsImp
import Env
import ErrM
import PrintImp
import Matching

import Data.Map hiding (map)
import qualified Data.Map as Map
import Debug.Trace (trace)

import Control.Monad (foldM, foldM_, unless, zipWithM_, zipWithM)

typecheck :: Prog -> Err ()
typecheck (Prog defs) = do
      env <- foldM recolectarDataTypesyFunciones emptyEnv defs
      checkProg env (Prog defs)

recolectarDataTypesyFunciones :: Env -> Def -> Err Env
recolectarDataTypesyFunciones env (DataType idDataType tps decls) = 
      updateData env idDataType (extractTypeVars tps, convertTypesParamsDecl (extractTypeVars tps) decls)
recolectarDataTypesyFunciones env (Method idMethod tps arg (Param _ ret) _) = 
      updateFun env idMethod (extractTypeVars tps, 
      map (convertTypesParams (extractTypeVars tps)) (extractParams arg),
      convertTypesParams(extractTypeVars tps) ret)

convertTypesParamsDecl :: [Id] -> [Decl] -> [Decl]
convertTypesParamsDecl typVars decls = map convertDecl decls
  where
    convertDecl :: Decl -> Decl
    convertDecl (Decl id types) = Decl id (convertTypes types)
    convertTypes :: Types -> Types
    convertTypes TypesEty = TypesEty
    convertTypes (TypesSeq ts) = TypesSeq (map (convertTypesParams typVars) ts)

checkProg :: Env -> Prog -> Err () 
checkProg env (Prog defs) = mapM_ (checkDef env) defs

extractTypeVars :: Tps -> [Id]
extractTypeVars TypeParEty = []
extractTypeVars (TypeParSeq vs) = vs

extractParams :: [Param] -> [Type]
extractParams = map (\ (Param _ ty) -> ty)

extractParam :: Param -> Type
extractParam (Param id ty) = ty

-- | Convert in type variables type ids with empty arguments and declared as type params vars (in list parameter)
convertTypesParams :: [Id] -> Type -> Type
convertTypesParams typVars (Type t TypesEty)
      | t `elem` typVars = TypeVar t
      | otherwise = Type t TypesEty
convertTypesParams typVars (Type t (TypesSeq []))
      | t `elem` typVars = TypeVar t
      | otherwise = Type t TypesEty
convertTypesParams typVars (Type t (TypesSeq typs)) = Type t (TypesSeq $ map (convertTypesParams typVars) typs)
convertTypesParams typVars (TypeVar t) = TypeVar t

checkDef :: Env -> Def -> Err ()
checkDef env (DataType idDataType tps decls) = do
      -- Agrego variables polimorficas a TypeParams
      env <- return (updateTypeParams env (extractTypeVars tps))
      -- Chequeo los tipos de los argumentos de los constructores
      let typedecls = convertTypesParamsDecl (extractTypeVars tps) decls
      mapM_ (checkDecl env (extractTypeVars tps)) typedecls
      return ()
checkDef env (Method idMethod tps params retType stats) = do
      -- Agrego variables polimorficas a TypeParams
      env2 <- return (updateTypeParams env (extractTypeVars tps))
      -- Creo un nuevo contexto
      env3 <- return (newBlock env2)
      -- Agrego variables en el contexto (params = ((Id, ty)))
      env4 <- foldM (\env' (Param id ty) -> updateVar env' id (convertTypesParams (extractTypeVars tps) ty)) env3 (params ++ [retType])
      -- Chequeo tipo de retorno
      checkType env4 ( (convertTypesParams (extractTypeVars tps) ( extractParam retType)))
      -- Chequeo variables de argumentos de función
      mapM_ (\param -> checkType env4 (convertTypesParams (extractTypeVars tps) (extractParam param))) params
      -- Chequeo Statements
      finalEnv <- checkStms env4 stats
      return ()

checkDecl :: Env -> [Id] -> Decl -> Err ()
checkDecl env typVars (Decl id types) = 
  case types of
    TypesEty -> return ()
    TypesSeq ts -> 
      mapM_ (\t -> let convertedType = convertTypesParams typVars t
                   in checkType env convertedType) ts

checkType :: Env -> Type -> Err ()
checkType env (TypeVar idTypeVar) = do
    lookupTypeParam env idTypeVar
checkType env (Type idType types) = do
    -- Verifico que idType está declarado en DataTypes
    typesInDataTypes <- lookupDataType env idType
    -- Verifico que |types| == |types In DataTypes|
    unless ((length typesInDataTypes) == (length (typesToList types))) $ fail "|types| != |types In DataTypes|"
    -- Verifico recursivamente
    checkTypes env types

checkTypes :: Env -> Types -> Err ()
checkTypes env TypesEty = return () 
checkTypes env (TypesSeq ts) = mapM_ (checkType env) ts     

typesToList :: Types -> [Type]
typesToList TypesEty = []
typesToList (TypesSeq ts) = ts

checkStm :: Env -> Stat -> Err Env
checkStm env (StmBlock stats) = do
      -- Agrego nuevo context en tope de stack
      let envWithBlock = newBlock env
      -- Proceso recursivamente el resto de stats
      checkStms envWithBlock stats
checkStm env (StmVarDecl params) = do
      -- Cheqeuo que los tipos en params sean correctos
      let typarams = lookupTypeParams env
      mapM_ (checkType env) (map (convertTypesParams typarams) (extractParams params))

      -- Agrego las variables declaradas (params = [Param Id Type]) en el tope de mi stack Contexts
      foldM (\env' (Param id ty) -> updateVar env' id (convertTypesParams typarams ty)) env params
checkStm env (StmAssignEty) = Ok env 
checkStm env (StmAssignNEty ids exps) = do
      -- Chequeo que lenght ids == lenght exps
      if (length ids /= length exps)
            then Bad "length ids != length exps."
            else do
                  -- Chequeo variables declaradas en Contexts
                  typesVars <- mapM (lookupVar env) ids 
                  -- Infiero tipos de las expresiones en exps
                  typesExps <- mapM (inferExp env) exps 
                  -- Verifico igualdad de tipos entre variables y expresiones
                  if (equalTypes typesVars typesExps) 
                        then Ok env
                        else Bad ("Tipos incompatibles.\n" ++ "typesVars: " ++ show typesVars ++ "\ntypesExps: " ++ show typesExps)
checkStm env (StmCase id branchs) = do
      -- Chequeo que id declarada en Contexts
      typeX <- lookupVar env id
      -- Chequeo que todos los constructores en branchs estén declarados
      mapM_ (ckechBranch env typeX) branchs
      return env
checkStm env (StmWhile id branchs) = do
      -- Chequeo que id declarada en Contexts
      typeX <- lookupVar env id
      -- Chequeo que todos los constructores en branchs estén declarados
      mapM_ (ckechBranch env typeX) branchs
      return env

ckechBranch :: Env -> Type -> Branch -> Err ()
ckechBranch env (TypeVar t) (Branch left stats) = 
      Bad "No se pueden hacer casos en una variable polimorfica."
ckechBranch env (Type t betas) (Branch left stats) = do
      let (const, params) = case left of
            (LeftConstrEty id) -> (id,[])
            (LeftConstr id ids) -> (id,ids)
      (alphas, idTypeConst) <- lookupConstructor env const
      if (t /= idTypeConst) then
            Bad "No coinciden los tipos."
      else do {
            if ((length alphas) /= (length params)) then
                  Bad "lenght alphas != lenght params."
            else do
                  pi <- (lookupDataType env t)
                  subst <- matchList (extractTypes betas) (map TypeVar pi)
                  let nuevosAlphas = map (applySubst subst) alphas 
                  env'' <- foldM (\ env' (var, typ) -> updateVar env' var typ) env (zip params nuevosAlphas) 
                  -- Chequea los stats 
                  mapM_ (checkStm env'') stats
                  return ()
      }

checkStms :: Env -> [Stat] -> Err Env
checkStms env stats = foldM checkStm env stats

inferExp :: Env -> Exp -> Err Type
inferExp env (ExpVar idExp) = (lookupVar env idExp)
inferExp env (ExpConstr idExp exps) = do
      -- Verifico que idExp está declarado en Constructors
      (alphas, idTypeConstructor) <- lookupConstructor env idExp
      betas <- mapM (inferExp env) exps
      -- Compruebo que los betas son instancias de los alphas y aplico la substitución
      subst <- matchList betas alphas
      -- Construyo el tipo de retorno
      idTypeRetorno <- lookupDataType env idTypeConstructor
      let params = map (applySubst subst) (map TypeVar idTypeRetorno)
      --return (applySubst subst (Type idTypeRetorno (TypesSeq alphas)))
      return (applySubst subst (Type idTypeConstructor (TypesSeq params)))
inferExp env (ExpCall idExp exps) = do
      -- Verifico que idExp está declarado en sig
      (_, alphas, alphaRet) <- lookupFun env idExp
      betas <- mapM (inferExp env) exps
      -- Compruebo que los betas son instancias de los alphas y aplico la substitución en alphaRetorno
      subst <- matchList betas alphas
      return (applySubst subst alphaRet) 

equalType :: Type -> Type -> Bool
equalType (Type t1 typs1) (Type t2 typs2) = t1 == t2 && extractTypes typs1 == extractTypes typs2
equalType t1 t2 = t1 == t2

equalTypes :: [Type] -> [Type] -> Bool
equalTypes [] [] = True 
equalTypes (t1:ts1) (t2:ts2) = (equalType t1 t2) && (equalTypes ts1 ts2)

-- Funciones auxiliares 
-- Check multiple substitutions are compatible and join them
unionSubsts :: [Subst] -> Err Subst
unionSubsts = foldM unionCheck empty

matchList :: [Type] -> [Type] -> Err Subst
matchList betas alphas = do
      unless (length betas == length alphas) $ fail "Not unify because lengths are different"
      substs <- zipWithM match betas alphas
      unionSubsts substs
