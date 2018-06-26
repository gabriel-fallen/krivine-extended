module Main where

import Lib


term1 :: Term
term1 = App (Lam 1 (Var 0 0)) [Free "end"]

term2 :: Term
term2 = App (Lam 2 (Var 0 0)) [term1, Free "unused"]

term3 :: Term
term3 = App (Lam 2 term1) [Free "unused1", Free "unused2"]

term4 :: Term
term4 = App (Lam 1 (App (Var 0 0) [Var 0 0])) [Free "z"]

-- An example from Daniel's paper
term5 :: Term
term5 = App (Lam 1 $ App (Var 0 0) [Var 0 0]) [Lam 1 (Var 0 0)]

-- Church numerals
c0 :: Term
c0 = Lam 2 (Var 0 1)

c1 :: Term
c1 = Lam 2 (App (Var 0 0) [Var 0 1])

c2 :: Term
c2 = Lam 2 (App (Var 0 0) [App (Var 0 0) [Var 0 1]])

-- plus = \m n f x -> m f (n f x)
plus :: Term
plus = Lam 4 $ App (Var 0 0) [Var 0 2, App (Var 0 1) [Var 0 2, Var 0 3]]

add21 :: Term
add21 = App plus [c2, c1, Lam 1 $ App (Free "1+") [Var 0 0], Free "0"]

-- Perf test with large expressions

idt :: Term
idt = Lam 1 $ Var 0 0

nid :: Int -> Term
nid = (!!) $ iterate (App idt . (: [])) (Free "x")

testt :: Int -> Int -> Term
testt n m = App (Free "z") $ replicate m $ nid n


main :: IO ()
main = do
  let end = App (Free "end") []
      r1  = eval' term1
      r2  = eval' term2
      r3  = eval' term3
      r4  = eval' term4
      r5  = eval' term5
      r21 = eval' add21
      small = eval' $ testt 7 6
      big   = eval' $ testt 15000 2000
  -- print r1
  -- putStrLn $ if r1 == end then "pass" else "fail"
  -- print r2
  -- putStrLn $ if r2 == end then "pass" else "fail"
  -- print r3
  -- putStrLn $ if r3 == end then "pass" else "fail"
  -- print r4
  -- print r21
  -- print r5
  print big
