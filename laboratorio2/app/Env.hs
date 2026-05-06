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

lookupVar :: Env -> Id -> Err Type
lookupVar (sig, []) x = Bad "Variable not found"
lookupVar (sig, g:gs) x = case Map.lookup x g of
    Just t -> Ok t 
    Nothing -> lookupVar (sig, gs) x


lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun = undefined

updateVar :: Env -> Id -> Type -> Err Env
updateVar = undefined

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun = undefined

newBlock :: Env -> Env
newBlock = undefined

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])
