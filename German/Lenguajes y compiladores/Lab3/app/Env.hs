{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Env where

import AbsImp
import ErrM
import PrintImp

import Control.Monad
import Data.Map hiding (foldl, insert, map)
import qualified Data.Map as Map

-- | (Function signatures, Variables declarations, Data Types, Constructors)
type Env = (Sig,[Context], DataTypes, Constructors, TypeParameters)
-- | functions id -> (type variables params, type params, returned type) 
type Sig = Map Id ([Id], [Type], Type)
-- | variables id -> type
type Context = Map Id Type
-- | constructor id -> (type params, data type)
type Constructors = Map Id ([Type], Id)
-- | data types ids -> type variable params
type DataTypes = Map Id [Id]
-- | type parameters
type TypeParameters = [Id]

-- Pre: busca la variable en todos los contextos
lookupVar :: Env -> Id -> Err Type
lookupVar (sig,[],dataTypes,constructors,typeParameters) id =  Bad ("Variable no encontrada" ++ show id)              
lookupVar (sig,(ctx:ctxs),dataTypes,constructors,typeParameters) key =
    case Map.lookup key ctx of
        Just ty -> Ok ty
        Nothing -> lookupVar (sig,ctxs,dataTypes,constructors,typeParameters) key

lookupFun :: Env -> Id -> Err ([Id], [Type], Type)
lookupFun (sig,ctx,dataTypes,constructors,typeParameters) key =
    case Map.lookup key sig of
        Just function -> Ok function
        Nothing -> Bad "Función no encontrada"

lookupConstructor :: Env -> Id -> Err ([Type], Id)
lookupConstructor (sig,ctx,dataTypes,constructors,typeParameters) key =
    case Map.lookup key constructors of
                    Just constructor -> Ok constructor
                    Nothing -> Bad "Constructor no encontrado"

lookupDataType :: Env -> Id -> Err [Id]
lookupDataType (sig,ctx,dataTypes,constructors,typeParameters) key =
    case Map.lookup key dataTypes of
                    Just dataType -> Ok dataType
                    Nothing -> Bad ("DataType no encontrado" ++ show key) 

lookupTypeParam :: Env -> Id -> Err ()
lookupTypeParam (sig,ctx,dataTypes,constructors,[]) key = Bad "TypeParam no encontrado"
lookupTypeParam (sig,ctx,dataTypes,constructors,(x:xs)) key = 
    case (x == key) of 
        True -> return ()
        False -> lookupTypeParam (sig,ctx,dataTypes,constructors,xs) key

lookupTypeParams :: Env -> TypeParameters
lookupTypeParams (sig,ctx,dataTypes,constructors,t) =t 

-- Solo hace update de la variable en el contexto actual
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig,[],dataTypes,constructors,typeParameters) key ty = Bad "No hay contextos para actualizar"
updateVar (sig,(ctx:ctxs),dataTypes,constructors,typeParameters) key ty =
    case Map.lookup key ctx of
        Just ty -> Bad "La variable ya existe"
        Nothing -> 
            let updatectx = Map.insert key ty ctx
            in Ok (sig,(updatectx:ctxs),dataTypes,constructors,typeParameters)

updateFun :: Env -> Id -> ([Id], [Type], Type) -> Err Env
updateFun (sig,ctx,dataTypes,constructors,typeParameters) key function =
    case Map.lookup key sig of
        Just ty -> Bad "La función ya existe"
        Nothing -> Ok (Map.insert key function sig, ctx, dataTypes, constructors, typeParameters)

updateConstructor :: Constructors -> Id -> ([Type], Id) -> Err Constructors
updateConstructor cons key con = 
    case Map.lookup key cons of
        Just con -> Bad "El constructor ya existe"
        Nothing -> Ok (Map.insert key con cons)

-- Inserta [id] en DataTypes y [Decl] en Constructors
updateData :: Env -> Id -> ([Id], [Decl]) -> Err Env
updateData (sig, ctx, dataTypes, constructors, typeParameters) key (ids, decls) =
    case Map.lookup key dataTypes of
        Just _ -> Bad "DataType ya existe"
        Nothing -> 
            let newDataTypes = Map.insert key ids dataTypes
            in case (foldM insertDecl constructors decls) of
                Bad err -> Bad err
                Ok newConstructors -> Ok (sig, ctx, newDataTypes, newConstructors, typeParameters)
    where
    insertDecl :: Constructors -> Decl -> Err Constructors
    insertDecl cons (Decl constId types) = 
        let typeList = extractTypes types
        in updateConstructor cons constId (typeList, key)

updateTypeParams :: Env -> TypeParameters -> Env
updateTypeParams (sig, contexts, dataTypes, constructors, _) typeParameters =
    (sig, contexts, dataTypes, constructors, typeParameters)

extractTypes :: Types -> [Type]
extractTypes TypesEty = []
extractTypes (TypesSeq ts) = ts

newBlock :: Env -> Env
newBlock (sig, contexts,dataTypes,constructors,typeParameters) = (sig,  empty : contexts, dataTypes, constructors, typeParameters)

emptyEnv :: Env
emptyEnv = (empty, [empty], empty, empty,[])