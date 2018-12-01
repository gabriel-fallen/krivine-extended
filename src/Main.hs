module Main where

import Control.DeepSeq
import Lib
import Parser


term1 :: Term
term1 = App (Lam (Var 0)) $ Free "end"

term2 :: Term
term2 = App (Lam $ App (Lam (Var 1)) (Free "unused")) term1

term3 :: Term
term3 = App (App (Lam term1) (Free "unused1")) $ Free "unused2"

term4 :: Term
term4 = App (Lam (App (Var 0) $ Free "unused")) $ Free "z"

-- An example from Daniel's paper
term5 :: Term
term5 = App (Lam $ App (Var 0) $ Var 0) $ Lam (Var 0)

-- Church numerals
c0 :: Term
c0 = Lam $ Lam (Var 0)

c1 :: Term
c1 = Lam $ Lam (App (Var 1) $ Var 0)

c2 :: Term
c2 = Lam $ Lam (App (Var 1) $ App (Var 1) $ Var 0)

-- plus = \m n f x -> m f (n f x)
plus :: Term
plus = Lam $ Lam $ Lam $ Lam $ App (App (Var 3) $ Var 1) $ App (App (Var 2) $ Var 1) $ Var 0

add21 :: Term
add21 = App (App (App (App plus c2) c1) (Lam $ App (Free "1+") $ Var 0)) $ Free "0"

add21' :: Term
add21' = App (App plus c2) c1

-- Perf test with large expressions

idt :: Term
idt = Lam $ Var 0

nid :: Int -> Term
nid = (!!) $ iterate (App idt) (Free "x")

testt :: Int -> Int -> Term
testt n m = foldl App (Free "z") $ replicate m $ nid n

--

-- | church k = \f.\z.(f)^{k}z -- Church numeral @k@
church :: Int -> Term
church k = Lam $ Lam $ iterate (App (Var 1)) (Var 0) !! k

nmpair :: Int -> Int -> Term
nmpair n m = App (App t $ u n) $ u m
  where
    -- t = \x.\y.((a)(x)y)(b)(y)x
    t = Lam $ Lam $ App (App (Free "a") $ App (Var 1) (Var 0)) $ App (Free "b") $ App (Var 0) (Var 1)
    u = church

klmn :: Int -> Int -> Int -> Int -> Term
klmn k l m n = App (App (App (App t $ u k) $ u l) $ u m) $ u n
  where
    -- t = \x1.\x2.\x3.\x4.((((a)(x1)x2) (b)(x2)x1) (c)(x3)x4) (d)(x4)x3
    t = Lam $ Lam $ Lam $ Lam $ foldl App (Free "a") [App (Var 3) (Var 2), App (Free "b") $ App (Var 2) (Var 3), App (Free "c") $ App (Var 1) (Var 0), App (Free "d") $ App (Var 0) (Var 1)]
    u = church

sixTuple :: Int -> Int -> Term
sixTuple n m = App t u
  where
    t = Lam $ foldl App (Free "y") [Var 0, Var 0, Var 0, Var 0, Var 0, Var 0]
    u = App (App (Free "x") $ App (church n) (church m)) $ App (church m) (church n)

-- ((x)((\\(((2)\\(1)2)\\\(2)1)((1)\\(1)2)\\\1)(y1)y2)(y3)y4)((\\(((2)\\(1)2)\\\(2)1)((1)\\(1)2)\\\1)(y5)y6)(y7)y8

parse :: String -> String
parse = either id show . parseDeBruijnString

evaluate :: String -> String
evaluate = either id (show . eval') . parseDeBruijnString

main :: IO ()
main = do
  let end    = Free "end"
      r1     = eval' term1
      r2     = eval' term2
      r3     = eval' term3
      r4     = eval' term4
      r5     = eval' term5
      r21    = eval' add21
      r21'   = eval' add21'
      big    = eval' $ testt 15000 2000
      pair23 = eval' $ nmpair 2 3
      pair76 = eval' $ nmpair 7 6
      klmn8796 = eval' $ klmn 8 7 9 6
      klmn6776 = eval' $ klmn 6 7 7 6
      tuple86  = eval' $ sixTuple 9 6
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
  -- print $ eval' $ App (Lam $ Lam $ App (Var 0) (Var 1)) (Free "z")
  -- print $ eval' $ App c2 c2
  -- print big
  -- print pair23 -- for visual examination
  -- pair76 `deepseq` putStrLn "Done."
  -- klmn8796 `deepseq` putStrLn "Done."
  -- klmn6776 `deepseq` putStrLn "Done."
  -- print $ sixTuple 2 3
  -- tuple86 `deepseq` putStrLn "Done."
  interact parse
