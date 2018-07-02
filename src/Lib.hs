{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module Lib where

import Control.DeepSeq
import Control.Parallel.Strategies
import Debug.Trace
import GHC.Generics (Generic)


data Term
  = Var Int Int       -- <n, m> de Bruijn index variant
  | Lam Int Term      -- \x1 .. \xn -> term where term doesn't start with lambda
  | App Term [Term]
  | Free String
  deriving (Eq, Generic, NFData)

instance Show Term where
  show (Var m n) = '<' : show m ++ ", " ++ show n ++ ">"
  show (Lam n t) = 'λ' : show n ++ ". " ++ show t
  show (App u v) = '(' : show u ++ ") " ++ show v
  show (Free n)  = n

  
data Closure = Closure
  { getTerm :: Term
  , getEnv  :: Env
  } deriving (Eq, Show, Generic, NFData)

data Env
  = Nil
  | Lift Env
  | Env [Closure] Env
  deriving (Eq, Show, Generic, NFData)

type Stack = [Closure]


level :: Env -> Int
level Nil       = 0
level (Lift e)  = 1 + level e
level (Env _ e) = level e

-- | Adjust de Bruijn indexes when partially applying.
shiftVars :: Int -> Int -> Term -> Term
-- shiftVars l k (App t u) = App (shiftVars l k t) $ map (shiftVars l k) u
-- shiftVars l k (Lam n t) = Lam n $ shiftVars (l + 1) k t
-- shiftVars l k v@(Var l1 k1) = if l == l1 && k1 >= k then Var l (k1 - k) else v
-- shiftVars _ _ t@(Free _) = t
shiftVars _ _ t = t


eval :: Env -> Stack -> Term -> Term
eval e s (App t u) = eval e s' t
  where
    s' = map (\x -> Closure x e) u ++ s

eval e s v@(Lam n t) = trace ("eval (" ++ show e ++ ") (" ++ show s ++ ") (" ++ show v ++ ")") $
  let m = length s in
  if m < n
    -- when lambda is applied to too few arguments leave lambda in place
    then Lam (n - m) $ shiftVars 0 m $ eval (Lift e) s' t -- FIXME: s' is always [] in this case
    else eval e' s' t
  where
    e' = Env cs e
    (cs, s') = splitAt n s

eval e s v@(Var n m) = trace ("eval (" ++ show e ++ ") (" ++ show s ++ ") (" ++ show v ++ ")") $
  trace ("Level = " ++ show (level e)) $
  case fetch e n m of
  Nothing             -> case length s of
    0 -> Var (n + level e - 1) m -- no application
    1 -> App (Var (n + level e - 1) m) $ map (\c -> eval (getEnv c) [] $ getTerm c) s -- no parallelism
    _ -> App (Var (n + level e - 1) m) (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq) -- FIXME: is this correct?
  Just (Closure t e') -> eval e' s t 

-- the special case
eval e s t@(Free _) = App t (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq)


fetch :: Env -> Int -> Int -> Maybe Closure
fetch Nil _ _        = Nothing
fetch (Env cs _) 0 m
  | m < length cs    = Just $ cs !! m
  | otherwise        = Nothing
fetch (Env _ ps) n m = fetch ps (n - 1) m
fetch (Lift e) n m   = do
  Closure t e' <- fetch e n m
  pure $ Closure t (Lift e')

eval' :: Term -> Term
eval' = eval Nil []
