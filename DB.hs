module DB where


import Context

import System.Process (callCommand)
import Data.Char (isUpper)

------------------------------------------------------
-- Sample complex program: graph matching, query and transformation
------------------------------------------------------
 
-- graph database = set of edges/triples, see "db.png"
type Edge  = (String,String,String)
type Graph = [Edge]

db,query :: Graph
db = [ ("laurent","hasskill","computerScience")
     , ("laurent","workfor","ensisa")
     , ("ensisa","at","mulhouse")
     , ("zhao","hasskill","computerScience")
     , ("zhao","hasdirector","laurent")
     ]

query = [ ("X","hasskill","computerScience")
        , ("X","workfor","ensisa")
        ]

-- generate Graphviz view
-- display "db" db
-- display "query" query
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

v1 = match1 "X" "laurent" ctx
-- (True,[("X","laurent")])

match3 :: Edge -> Edge -> Context -> (Bool,Context)
match3 (p,q,r) (v,w,x) ctx = 
 let (r1,ctx1) = match1 p v ctx in
 if r1 then 
   let (r2,ctx2) = match1 q w ctx1 in
   if r2 then
     match1 r x ctx2
    else (False,ctx)
  else (False,ctx)

v3 = match3 ("laurent","X","Y") ("laurent","hasskill","computerScience")  ctx
-- (True,[("Y","computerScience"),("X","hasskill")])

matchn :: Edge -> Graph -> Context -> [(Bool,Context)]
matchn p vs ctx = filter fst (map (\v -> match3 p v ctx) vs) 

vn = map snd (matchn ("laurent","X","Y") db ctx)
-- [[("Y","computerScience"),("X","hasskill")],[("Y","ensisa"),("X","workfor")]]

answer :: Graph -> Graph -> Context -> [(Bool,Context)]
answer []     vs ctx = [(True,ctx)]
answer (p:ps) vs ctx = 
 let cs = map snd (matchn p vs ctx) in concat (map (answer ps vs) cs)

v = map snd (answer query db ctx)
-- [[("X","laurent")]]


