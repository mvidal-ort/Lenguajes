--Germán Gómez
--319482


{-# LANGUAGE CPP #-}

module Env where

import AbsCPP
import PrintCPP
import ErrM

import Data.Map as Map

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
import Control.Monad hiding (fail)

fail = Bad
#endif

type Env = (Sig,[Context])
type Sig = Map Id ([Type],Type)
type Context = Map Id Type

--Pre: busca la variable en todos los contextos --Ver si está bien
lookupVar :: Env -> Id -> Err Type
lookupVar (sig,[empty]) _ =  Bad "Variable no encontrada"              
lookupVar (sig,ctx:ctxs) key =
                case Map.lookup key ctx of
                    Just ty -> Ok ty
                    Nothing -> lookupVar (sig,ctxs) key

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig,ctx) key =
            case Map.lookup key sig of
                Just function -> Ok function
                Nothing -> Bad "Función no encontrada"

--Pre: Solo hace update de la variable en el contexto actual
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, [empty]) key ty = Bad "No hay contextos para actualizar"
updateVar (sig, ctx:ctxs) key ty =
    let updatedCtx = Map.insert key ty ctx
    in Ok (sig, updatedCtx : ctxs)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig,ctx) key function =
    Ok (Map.insert key function sig, ctx)

newBlock :: Env -> Env
newBlock (sig, contexts) = (sig,  empty : contexts)

emptyEnv :: Env
emptyEnv = (empty, [empty])
