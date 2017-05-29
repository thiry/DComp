{-# LANGUAGE BangPatterns #-}

module DBacc where
-- try optimization: cut, accu

import Context

import System.Process (callCommand)
import Data.Char (isUpper)

------------------------------------------------------
-- Sample complex program: graph matching, query and transformation
------------------------------------------------------
 
-- graph database = set of edges/triples, see "db.png"
type Edge  = (String,String,String)
type Graph = [Edge]

-- generate Graphviz view
view file db = writeFile (file++".dot") ("digraph G {\nnode[shape=plaintext]; edge[arrowhead=vee];\n"++(concat (map (\(a,b,c)-> (show a)++"->"++(show c)++"[label="++(show b)++"];\n") db))++"}")

display file db = do
 view file db
 callCommand ("dot -Tpng "++file++".dot -o "++file++".png")

-- matching
--isVariable p = isUpper (p!!0)
isVariable p = (p!!0)=='?'
--isVariable p = (p!!0)=='_'

match1 :: String -> String -> Context -> (Bool,Context)
match1 p v ctx = 
 if (isVariable p) then 
   if (has p ctx) then ((get p ctx)==v,ctx)
    else (True,put p v ctx)
  else (p==v,ctx)

match3 :: Edge -> Edge -> Context -> (Bool,Context)
match3 (p,q,r) (v,w,x) ctx = 
 let (r1,ctx1) = match1 p v ctx in
 if r1 then 
   let (r2,ctx2) = match1 q w ctx1 in
   if r2 then
     match1 r x ctx2
    else (False,ctx)
  else (False,ctx)

--matchn :: Edge -> Graph -> Context -> [(Bool,Context)]
--matchn p vs ctx = filter fst (map (\v -> match3 p v ctx) vs)

--matchn p []     ctx = []
--matchn p (v:vs) ctx = 
 --let (r1,r2) = match3 p v ctx
     --rs      = matchn p vs ctx
  --in if r1 then (r1,r2):rs else rs
  
matchn :: Edge -> [Edge] -> Context -> [Context]
matchn p vs ctx = matchnacc p vs ctx []

matchnacc p []     ctx acc = acc
matchnacc p (v:vs) ctx acc = 
 let (r1,r2) = match3 p v ctx
  in if r1 then matchnacc p vs ctx $! (r2:acc) else matchnacc p vs ctx acc

--answer :: Graph -> Graph -> Context -> [(Bool,Context)]
--answer []     vs ctx = [(True,ctx)]

--answer (p:[]) vs ctx = matchn p vs ctx
--answer (p:ps) vs ctx = concat (map (answer ps vs) (matchn p vs ctx))

answer ps vs ctx = answerAcc ps vs ctx []

answerAcc (p:[]) vs ctx acc = matchn p vs ctx
answerAcc (p:ps) vs ctx acc = answerAcc ps vs ctx $! (uniMap' (matchn p vs) acc)

uniMap f []     = []
uniMap f (x:xs) = (f x)++(uniMap f xs)

uniMap' f xs = uniMapAcc f xs []

uniMapAcc f []     r = r
uniMapAcc f (x:xs) r = uniMapAcc f xs $! ((f x)++r)
