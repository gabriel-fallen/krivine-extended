{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module Lib where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List (foldl')
import Debug.Trace
import GHC.Generics (Generic)


data Term
  = Var  !Int        -- de Bruijn index
  | Lam  !Term
  | App  !Term !Term
  | Free !String
  deriving (Eq, Generic, NFData)

instance Show Term where
  show (Var n)   = '<' : show n ++ ">"
  show (Lam t)   = "λ. " ++ show t
  show (App u v) = '(' : show u ++ ") " ++ show v
  show (Free n)  = n

size :: Term -> Integer
size (Var _)   = 1
size (Lam t)   = 1 + size t
size (App u v) = 1 + size u + size v
size (Free _)  = 1

  
data Closure = Closure
  { getTerm :: !Term
  , getEnv  :: !Env
  } deriving (Eq, Show, Generic, NFData)

data Env
  = Nil
  | Lift !Env
  | Env !Closure !Env
  deriving (Eq, Show, Generic, NFData)

type Stack = [Closure]


level :: Env -> Int
level Nil       = 0
level (Lift e)  = 1 + level e
level (Env _ e) = level e


eval :: Env -> Stack -> Term -> Term
eval !e !s (App t u) = eval e s' t
  where
    s' = Closure u e : s

eval !e !s v@(Lam t) = -- trace ("eval (" ++ show e ++ ") (" ++ show s ++ ") (" ++ show v ++ ")") $
  if null s
    -- evaluate under lambda when not applied
    then Lam $ eval (Env nilClosure $ Lift e) [] t
    else eval e' s' t
  where
    e' = Env c e
    (c : s') = s
    nilClosure = Closure (Var 0) Nil

eval !e !s v@(Var n) = -- trace ("eval (" ++ show e ++ ") (" ++ show s ++ ") (" ++ show v ++ ")") $
  -- trace ("Level = " ++ show (level e)) $
  case fetch e n of
  Nothing             -> case length s of
    0 -> Var (n + level e) -- no application
    1 -> App (Var $ n + level e) $ let c = head s in eval (getEnv c) [] $ getTerm c -- no parallelism
    _ -> foldl' App (Var $ n + level e) (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq) -- FIXME: is this correct?
  Just (Closure t e') -> eval e' s t 

-- the special case
eval !e !s t@(Free _) = if null s
  then t
  else foldl' App t (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq)


fetch :: Env -> Int -> Maybe Closure
fetch Nil _       = Nothing
fetch (Env c _) 0 = Just c
fetch (Env _ p) n = fetch p (n - 1)
fetch (Lift e)  n = do
  Closure t e' <- fetch e n
  pure $ Closure t (Lift e')

eval' :: Term -> Term
eval' = eval Nil []
