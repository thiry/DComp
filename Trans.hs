module Trans where

import DB
import Context

-- Complement: evaluate pattern and graph transformations
eval1 :: String -> Context -> String
eval1 x ctx = if (isVariable x) then get x ctx else x

eval3 :: Edge -> Context -> Edge
eval3 (p,q,r) ctx = (eval1 p ctx, eval1 q ctx, eval1 r ctx)

evaln :: Graph -> Context -> Graph
evaln g ctx = map (\e -> eval3 e ctx) g

--trans :: Graph -> Graph -> Graph -> Graph
trans pre post vs = -- pushout
 let cs = map snd (answer pre db ctx) 
     ds = concat (map (evaln pre) cs)
     vs'= filter (`notElem` ds) vs
     ds'= concat (map (evaln post) cs)
  in vs'++ds'

db2 = trans [("X","at","Y")] [("X","hasLocation","Y")] db -- renaming
db3 = trans [] [("michel","workfor","ensise")] db         -- adding