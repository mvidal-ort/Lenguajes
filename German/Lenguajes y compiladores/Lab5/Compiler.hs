module CodeGen where

import AbsER
import Data.List (intercalate)
import Control.Monad.State

-- Tipo para representar un NFA
data NFA = NFA {
    start :: String,             -- Estado inicial
    transitions :: [Transition], -- Lista de transiciones
    end :: String                -- Estado final
} deriving Show

type Transition = (String, String, String) -- (q, símbolo, q')

-- Generar código NFA a partir de un programa ER
generateNFA :: Prog -> String
generateNFA (Prog _ expRegs) =
    intercalate "\n" (map generateExpRegNFA expRegs)

-- Generar el NFA de una definición ExpReg
generateExpRegNFA :: ExpReg -> String
generateExpRegNFA (ExpReg (ExpName expName) exp _) =
    let nfa = evalState (expToNFA exp) 0
    in "automata " ++ expName ++ ",\n" ++
       transitionsToString (transitions nfa) ++
       "end"

-- Convertir transiciones a String
transitionsToString :: [Transition] -> String
transitionsToString = intercalate "\n" . map showTransition
  where
    showTransition (q1, r, q2) = q1 ++ " : " ++ r ++ " goto " ++ q2

-- Monada State para generar estados únicos
type GenState a = State Int a

newState :: GenState String
newState = do
    n <- get
    put (n + 1)
    return ("q" ++ show n)

-- Convertir Exp a NFA
expToNFA :: Exp -> GenState NFA
expToNFA expr = case expr of
    Evacio -> do
        s <- newState
        e <- newState
        return $ NFA s [] e
    EPalabraVacia -> do
        s <- newState
        e <- newState
        return $ NFA s [(s, "eps", e)] e
    Esymbol (Symbol sym) -> do
        s <- newState
        e <- newState
        return $ NFA s [(s, "'" ++ sym ++ "'", e)] e
    EConcat exp1 exp2 -> do
        nfa1 <- expToNFA exp1
        nfa2 <- expToNFA exp2
        return $ NFA (start nfa1)
                     (transitions nfa1 ++ [(end nfa1, "eps", start nfa2)] ++ transitions nfa2)
                     (end nfa2)
    EUnion exp1 exp2 -> do
        nfa1 <- expToNFA exp1
        nfa2 <- expToNFA exp2
        s <- newState
        e <- newState
        return $ NFA s
                     ([(s, "eps", start nfa1), (s, "eps", start nfa2)]
                      ++ transitions nfa1
                      ++ transitions nfa2
                      ++ [(end nfa1, "eps", e), (end nfa2, "eps", e)])
                     e
    EIter exp1 -> do
        nfa <- expToNFA exp1
        s <- newState
        e <- newState
        return $ NFA s
                     ([(s, "eps", start nfa), (s, "eps", e),
                       (end nfa, "eps", start nfa), (end nfa, "eps", e)]
                      ++ transitions nfa)
                     e
    ETyped exp _ -> expToNFA exp