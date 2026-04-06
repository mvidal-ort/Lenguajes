module Matching where

import AbsImp
import Env (extractTypes)
import ErrM

import Control.Monad
import Data.Map hiding (foldl, map)

type Subst = Map Id Type

match :: Type -> Type -> Err Subst
match t (TypeVar v) = return (singleton v t)
match (TypeVar v) t = return (singleton v t)
match (Type f ftyps) (Type g gtyps)
  | f == g && length ts == length ts' = noNothings (zipWith match ts ts') >>= append
  where
    ts = extractTypes ftyps
    ts' = extractTypes gtyps
    noNothings :: [Err Subst] -> Err [Subst]
    noNothings = foldM (\ acc m -> fmap (:acc) m) []
    append :: [Subst] -> Err Subst
    append = foldM unionCheck empty
match t1 t2 = fail $ "Type " ++ show  t1 ++ " is not an instance of " ++ show t2      

unionCheck :: Subst -> Subst -> Err Subst
unionCheck s1 s2
  | intersection s1 s2 == intersection s2 s1 = return $ union s1 s2
  | otherwise = fail $ "Incompatible substitutions " ++ show s1 ++ " and " ++ show s2

-- >>> match (Type (Id "List") (TypesSeq [TypeVar (Id  "a")])) (Type (Id "List") (TypesSeq [Type (Id "N") TypesEty])) 
-- Right (fromList [(Id "a",Type (Id "N") TypesEty)])

applySubst :: Subst -> Type -> Type
applySubst s (TypeVar v) = findWithDefault (TypeVar v) v s
applySubst s t@(Type f TypesEty) = t
applySubst s t@(Type f (TypesSeq ts)) = Type f (TypesSeq $ map (applySubst s) ts)

-- check unification of found substitution
unify t1 t2 = fmap ((== t1) . flip applySubst t2)  (match t1 t2)

-- >>> unify (Type (Id "List") (TypesSeq [Type (Id "N") TypesEty])) (Type (Id "List") (TypesSeq [TypeVar (Id  "a")]))
-- Right True

      
