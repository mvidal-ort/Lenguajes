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

-- el uso return es mas apropiado, ya que la funcion return ya hace el casteo del tipo generico a al tipo generico Maybe a
-- Entonces el just y el Nothing ya estan considerados al poner return
-- si uso OK o Bad, tengo que especificarlos siendo ok el just, y el bad el nothing. Con reeturn no es necesario
-- return es una interfaz de mas alto nivel 


-- la busqueda es desde el contexto actual y sique hacia el contexto más exterior (hacia abajo en el stack)
lookupVar :: Env -> Id -> Err Type
lookupVar (sig, []) x = Bad "Variable not found"
lookupVar (sig, g:gs) x = case Map.lookup x g of
    Just t -> Ok t 
    Nothing -> lookupVar (sig, gs) x


lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, _) x = case Map.lookup x sig of
    Just f -> Ok f
    Nothing -> Bad "Function not found"

-- el update siempre es el en contexto mas arriba en el stack (actual)
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, g:gs) x t = case Map.lookup x g of
    Just _  -> Bad "Variable already declared in this scope"
    Nothing -> Ok (sig, Map.insert x t g : gs)

-- Aqui params representa todos los tipos de la funcion, tanto la lista de parametros de entrada como el de salida
updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, gs) f params = case Map.lookup f sig of
    Just _ -> Bad "Function already declared"
    Nothing -> Ok (Map.insert f params sig, gs)

newBlock :: Env -> Env
newBlock (sig, gs) = (sig, Map.empty : gs)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])


-- =====================
-- Ejemplo para probar
-- =====================

env0 :: Env
env0 =
  ( sig
  , [ ctxActual
    , ctxGlobal
    ]
  )
  where
    sig = Map.fromList
      [ (Id "printInt", ([Type_int], Type_void))
      , (Id "sum", ([Type_int, Type_int], Type_int))
      ]

    ctxActual = Map.fromList
      [ (Id "y", Type_bool)
      ]

    ctxGlobal = Map.fromList
      [ (Id "x", Type_int)
      , (Id "z", Type_double)
      ]