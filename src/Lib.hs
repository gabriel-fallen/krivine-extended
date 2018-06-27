{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lib where

import Control.DeepSeq
import Control.Parallel.Strategies
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


type Env = [[Closure]] -- Krivine environment is actually a list (stack) of lists of closures
  
data Closure = Closure
  { getTerm :: Term
  , getEnv  :: Env
  } deriving (Eq, Show, Generic, NFData)

type Stack = [Closure]


-- | Adjust de Bruijn indexes when partially applying.
shiftVars :: Int -> Int -> Term -> Term
shiftVars l k (App t u) = App (shiftVars l k t) $ map (shiftVars l k) u
shiftVars l k (Lam n t) = Lam n $ shiftVars (l + 1) k t
shiftVars l k v@(Var l1 k1) = if l == l1 && k1 >= k then Var l (k1 - k) else v
shiftVars _ _ t@(Free _) = t


eval :: Env -> Stack -> Term -> Term
eval e s (App t u) = eval e s' t
  where
    s' = map (\x -> Closure x e) u ++ s

eval e s (Lam n t) =
  let m = length s in
  if m < n
    -- when lambda is applied to too few arguments leave lambda in place
    then Lam (n - m) $ shiftVars 0 m $ eval e' s' t -- FIXME: s' is always [] in this case
    else eval e' s' t
  where
    e' = cs : e
    (cs, s') = splitAt n s

eval e s (Var n m) =
  case fetch e n m of
  Nothing             -> App (Var n m) (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq) -- FIXME: is this correct?
  Just (Closure t e') -> eval e' s t 

-- the special case
eval e s t@(Free _) = App t (map (\c -> eval (getEnv c) [] $ getTerm c) s `using` parList rdeepseq)


fetch :: Env -> Int -> Int -> Maybe Closure
fetch (cs : _) 0 m
  | m < length cs  = Just $ cs !! m
  | otherwise      = Nothing
fetch (_ : ps) n m = fetch ps (n - 1) m
fetch [] _ _       = Nothing

eval' :: Term -> Term
eval' = eval [] []
