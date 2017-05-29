module DB where


import Context
import Query

import System.Process (callCommand)
import Data.Char (isUpper)

import Data.Time (getCurrentTime,diffUTCTime)
import Control.Exception.Base (evaluate)

------------------------------------------------------
-- Sample complex program: graph matching, query and transformation
------------------------------------------------------
 
-- graph database = set of edges/triples, see "db.png"
type Edge    = (String,String,String)
type Graph   = [Edge]
type Pattern = Graph
type Result  = [(Bool,Context)]

-- generate Graphviz view
view file db = writeFile (file++".dot") ("digraph G {\nnode[shape=plaintext]; edge[arrowhead=vee];\n"++(concat (map (\(a,b,c)-> (show a)++"->"++(show c)++"[label="++(show b)++"];\n") db))++"}")

display file db = do
 view file db
 callCommand ("dot -Tpng "++file++".dot -o "++file++".png")

-- matching
isVariable p = (p!!0)=='?'

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

matchn :: Edge -> Graph -> Context -> Result
matchn p vs ctx = filter fst (map (\v -> match3 p v ctx) vs) 

answer :: Pattern -> Graph -> Context -> Result
answer (p:[]) vs ctx = matchn p vs ctx
answer (p:ps) vs ctx = 
 let cs = map snd (matchn p vs ctx) in concat (map (answer ps vs) cs)

--------------------
--- memorization ---
--------------------
type Mem = [(Pattern,Result)]

ctn :: Pattern -> Mem -> Bool
ctn x []         = False
ctn x ((p,v):cs) = if null w then ctn x cs else True
 where w = answer p x []

gt :: Pattern -> Mem -> Result
gt x ((p,v):cs) = if null w then gt x cs else v
 where w = answer p x []

pt :: Pattern -> Result -> Mem -> Mem
pt k v cs = (k,v):cs

answer' :: Mem -> Pattern -> Graph -> (Mem,Result)
answer' mem ps vs = if ctn ps mem then (mem,gt ps mem) else (mem',rs)
 where rs   = answer ps vs []
       mem' = pt ps rs mem

test = do
 db <- readFile "titanic.dhs"
 let graph = read db :: Graph
 let query1 = query "(?X Sex male)"
 let mem0 = []
 let (mem1,r1) = answer' mem0 query1 graph
 --print r1
 t1 <- getCurrentTime
 evaluate r1
 t2 <- getCurrentTime 
 putStrLn ("Wihout memorization: "++show (diffUTCTime t2 t1))
 let (mem2,r2) = answer' mem1 query1 graph
 t3 <- getCurrentTime  
 putStrLn ("Wihout memorization: "++show (diffUTCTime t3 t2))

