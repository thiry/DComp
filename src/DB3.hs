module DB3 where


--import Context
--import Query

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
--type Result  = [(Bool,Context)]

--isVariable x = (x!!0)=='?'

--match1 :: String -> String -> Context -> (Bool,Context)
match1 :: String -> String -> ([String],[String]) -> (Bool,([String],[String]))
match1 p@('?':vs) x ([],[])     = (True,([p],[x]))
match1 p@('?':vs) x (y:ys,z:zs) = if p==y 
 then (x==z ,(y:ys,z:zs))
  else let (r,(ys',zs')) = match1 p x (ys,zs) in (r,(y:ys',z:zs'))
match1 p          x (ys,zs) = (x==p,(ys,zs))

----match3 :: Edge -> Edge -> Context -> [Context]
match3 :: (String, String, String) -> (String, String, String) -> ([String], [String]) -> [([String], [[String]])]
match3 (p,q,r) (v,w,x) ctx = 
 let (r1,ctx1) = match1 p v ctx in
 if r1 then 
   let (r2,ctx2) = match1 q w ctx1 in
   if r2 then
     let (r3,(hs,vs)) = match1 r x ctx2 in
      if r3 then [(hs,[vs])] else []
    else []
  else []

----matchn :: Edge -> Graph -> Context -> [Context]
--matchn :: (String, String, String) -> [(String, String, String)] -> ([String], [String]) -> [([String], [String])]
matchn :: (String, String, String) -> [(String, String, String)] -> ([String], [String]) -> [([String], [[String]])]
matchn p []     ctx = []
matchn p (v:vs) ctx = 
 let r1 = matchn p vs ctx
     r2 = match3 p v ctx
  in case (r1,r2) of
      ([],r) -> r
      (r,[]) -> r
      ([(x,y)],[(x',y')]) -> [(x,y'++y)]

----answer :: Pattern -> Graph -> Context -> Result
--answer :: [(String, String, String)] -> [(String, String, String)] -> ([String], [String]) -> [([String], [[String]])]
answer [p] vs ctx = matchn p vs ctx
answer (p:ps) vs ctx = 
 let cs = matchn p vs ctx in
  case cs of
   []      -> []
   [(hs,ws)] -> merge (concat (map (\v -> answer ps vs (hs,v)) ws))

merge :: [([String], [[String]])] -> [([String],[[String]])]
merge []  = []
merge [x] = [x]
merge ((x1,x2):((y1,y2):zs)) = merge ((x1,y2++x2):zs)

db = [("a","b","c"),("a","b","e"),("c","f","g")]
q1 = ("?x","b","?y")
q2 = ("?y","f","g")

r1 = matchn q1 db ([],[]) -- == answer [q1] db []
-- == [(True,[("?y","b"),("?x","a")]),(True,[("?y","d"),("?x","a")])]
--r2 = map snd r1
--r3 = ((map fst).head $ r2,map (map snd) r2)
-- == (["?y","?x"],[["b","a"],["d","a"]])

{--
filter p xs       == concat (map (\x -> if p x then [x] else []) xs) 
map f (concat xs) == concat (map (map f) xs)
--}

--------------------
--- memorization ---
--------------------
--type Mem = [(Pattern,Result)]

--ctn :: Pattern -> Mem -> Bool
--ctn x []         = False
--ctn x ((p,v):cs) = if null w then ctn x cs else True
 --where w = answer p x []

--gt :: Pattern -> Mem -> Result
--gt x ((p,v):cs) = if null w then gt x cs else v
 --where w = answer p x []

--pt :: Pattern -> Result -> Mem -> Mem
--pt k v cs = (k,v):cs

--answer' :: Mem -> Pattern -> Graph -> (Mem,Result)
--answer' mem ps vs = if ctn ps mem then (mem,gt ps mem) else (mem',rs)
 --where rs   = answer ps vs []
       --mem' = pt ps rs mem

--test = do
 --db <- readFile "titanic.dhs"
 --let graph = read db :: Graph
 --let query1 = query "(?X Sex male)"
 --let mem0 = []
 --let (mem1,r1) = answer' mem0 query1 graph
 ----print r1
 --t1 <- getCurrentTime
 --evaluate r1
 --t2 <- getCurrentTime 
 --putStrLn ("Wihout memorization: "++show (diffUTCTime t2 t1))
 --let (mem2,r2) = answer' mem1 query1 graph
 --t3 <- getCurrentTime  
 --putStrLn ("Wihout memorization: "++show (diffUTCTime t3 t2))

