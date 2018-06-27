module Main where

import Control.DeepSeq
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

add21' :: Term
add21' = App plus [c2, c1]

-- Perf test with large expressions

idt :: Term
idt = Lam 1 $ Var 0 0

nid :: Int -> Term
nid = (!!) $ iterate (App idt . (: [])) (Free "x")

testt :: Int -> Int -> Term
testt n m = App (Free "z") $ replicate m $ nid n

nmpair :: Int -> Int -> Term
nmpair n m = App t [u n, u m]
  where
    -- t = \x.\y.((a)(x)y)(b)(y)x
    t = Lam 2 $ App (Free "a") [App (Var 0 0) [Var 0 1], App (Free "b") [App (Var 0 1) [Var 0 0]]]
    -- u k = \f.\z.(f)^{k}z -- Church numeral @k@
    u k = Lam 2 $ iterate (App (Var 0 0) . (: [])) (Var 0 1) !! k

klmn :: Int -> Int -> Int -> Int -> Term
klmn k l m n = App t [u k, u l, u m, u n]
  where
    -- t = \x1.\x2.\x3.\x4.((((a)(x1)x2) (b)(x2)x1) (c)(x3)x4) (d)(x4)x3
    t = Lam 4 $ App (Free "a") [App (Var 0 0) [Var 0 1], App (Free "b") [App (Var 0 1) [Var 0 0]], App (Free "c") [App (Var 0 2) [Var 0 3]], App (Free "d") [App (Var 0 3) [Var 0 2]]]
    -- u k = \f.\z.(f)^{k}z -- Church numeral @k@
    u k = Lam 2 $ iterate (App (Var 0 0) . (: [])) (Var 0 1) !! k
    

main :: IO ()
main = do
  let end    = App (Free "end") []
      r1     = eval' term1
      r2     = eval' term2
      r3     = eval' term3
      r4     = eval' term4
      r5     = eval' term5
      r21    = eval' add21
      r21'   = eval' add21'
      big    = eval' $ testt 15000 2000
      pair57 = eval' $ nmpair 5 7
      klmn5766 = eval' $ klmn 5 7 6 6
  -- print r1
  -- putStrLn $ if r1 == end then "pass" else "fail"
  -- print r2
  -- putStrLn $ if r2 == end then "pass" else "fail"
  -- print r3
  -- putStrLn $ if r3 == end then "pass" else "fail"
  -- print r4
  -- print r21
  -- print r21'
  -- print r5
  -- print big
  -- print pair56
  -- pair57 `deepseq` putStrLn "Done."
  klmn5766 `deepseq` putStrLn "Done."
