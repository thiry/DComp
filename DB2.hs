module DB2 where

import Context
import Query
import Data.List (nub)

-----------------------------------------------------
-- Transformation : Graph2 -> Graph
-----------------------------------------------------
--type Edge2  = (String,String,String)
--type Graph2 = [Edge2]

dom db = nub (map (\(x,_,_) -> x) db)

sel x db = (x,map (\(_,y,z) -> (y,z)) db')
 where db' = filter (\(x',_,_) -> x==x') db

--trf :: Graph2 -> Graph
trf db = map (\x -> sel x db) (dom db)

-----------------------------------------------------
-- New Matching
-----------------------------------------------------
type Graph   = [(String,[(String,String)])]
type Pattern = [(String,String,String)]

isVariable p = (p!!0)=='?'

match1 :: String -> String -> Context -> (Bool,Context)
match1 p v ctx = 
 if (isVariable p) then 
   if (has p ctx) then ((get p ctx)==v,ctx)
    else (True,put p v ctx)
  else (p==v,ctx)

match3 (p,q,r) (k,vs) ctx = 
 let (r1,ctx1) = match1 p k ctx in
 if r1 then 
   filter fst (map (\(v,s) -> let (r2,ctx2)=match1 q v ctx1 in if r2 then match1 r s ctx2 else (False,ctx)) vs)
  else []

matchn p []     ctx = []
matchn p@(t,_,_) (q:qs) ctx = 
 case match3 p q ctx of
  [] -> matchn p qs ctx
  r  -> if isVariable t then r++(matchn p qs ctx) else r

answer :: Pattern -> Graph -> Context -> [(Bool, Context)]
answer []     vs ctx = [(True,ctx)]
answer (p:ps) vs ctx = 
 let cs = map snd (matchn p vs ctx) in concat (map (answer ps vs) cs)


