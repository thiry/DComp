module Graph where -- Graph.hs

import Data.List (nub)
import Text.CSV (parseCSVFromFile)
import Data.Time (getCurrentTime,diffUTCTime)
import Control.Exception.Base (evaluate)
import System.Environment (getArgs)

import Query (query)

---
type Graph a b = [(a,b,a)]

g :: Graph String String
g = [("a","b","c"),("a","d","e"),("c","f","e")]

dom :: Eq a => Graph a a -> [a]
dom = nub.lay
 where lay []          = []
       lay ((x,y,z):g) = x:y:z:(lay g)

gmap :: (a->c,b->d) -> Graph a b -> Graph c d
gmap (fa,fb) = map (\(x,y,z) -> (fa x,fb y,fa z))

---
type Map a b = [(a,b)]

has :: Eq a => Map a b -> a -> Bool
has []        _ = False
has ((k,v):m) x = (x==k) || (has m x)

get :: Eq a => Map a b -> a -> b
get ((k,v):kv) x = if x==k then v else get kv x

put :: Map a b -> a -> b -> Map a b
put m k v = (k,v):m

rev :: Map a b -> Map b a
rev = map (\(x,y) -> (y,x))

---
vs = ["?x","?y","?z"]
n  = length vs

trf :: Graph String String -> (Map String Int,Graph Int Int)
trf g = (m,g')
 where m  = zip (vs++dom g) [1..]
       g' = gmap (get m,get m) g

r1 = trf g

q = [("a","?x","c")]

r2 = gmap (get m,get m) q
 where m = fst (trf g)

{--
> match1 1 12 []
[[(1,12)]]
> match3 (1,12,13) (19,12,13) []
[[(1,19)]]
> matchn (1,12,13) [(19,12,13),(15,12,13)] []
[[(1,19)],[(1,15)]]
> match [(1,12,13)] [(19,12,13),(15,12,13)] []
[[(1,19)],[(1,15)]]
--}
type Context = Map Int Int
isVar v = v <=n

match1 :: Int -> Int -> Context -> [Context]
match1 p v [] = 
 if isVar p then [[(p,v)]] 
  else if (p==v) then [[]] else []
match1 p v m = 
 if isVar p then 
   if (has m p) then (if (get m p)==v then [m] else []) else [(p,v):m]
  else if (p==v) then [m] else []

match3 :: (Int,Int,Int)->(Int,Int,Int)->Context -> [Context]
match3 (p1,p2,p3) (v1,v2,v3) m =
 case match1 p1 v1 m of
  []   -> []
  [r2] -> case match1 p2 v2 r2 of
           []   -> []
           [r3] -> case match1 p3 v3 r3 of
                    [] -> []
                    r  -> r

matchn :: (Int,Int,Int)->Graph Int Int->Context -> [Context]
matchn p []     m = []
matchn p (v:vs) m = case match3 p v m of
 []  -> matchn p vs m
 [r] -> r:(matchn p vs m)

match :: Graph Int Int -> Graph Int Int -> Context -> [Context]
match []     vs m = [m]
match (p:ps) vs m = concat (map (\m' -> match ps vs m') (matchn p vs m))

match0 ps vs = match ps vs []

---
readCSV :: String -> IO (Graph String String)
readCSV file = do
 content <- parseCSVFromFile file
 let Right tabl = content
 let headers    = head tabl
 let values     = init (tail tabl)
 let properties = map (\v -> zip headers v) values
 let transform ((_,s):ps) = map (\(p,v) -> (s,p,v)) ps
 let graph      = concat (map transform properties)
 return graph

sample = do
 g0 <- readCSV "titanic.csv"
 let (m,g) = trf g0
 evaluate g
 evaluate m
 let q1 = gmap (get m,get m) $ query "(?x Survived 1)"
 let r1 = match0 q1 g
 let q2 = gmap (get m,get m) $ query "(?y Survived 1) and (?x Sex male)"
 let r2 = match0 q2 g
 let q3 = gmap (get m,get m) $ query "(?x Survived ?z)"
 let r3 = match0 q3 g
 t1 <- getCurrentTime
 evaluate r1
 t2 <- getCurrentTime
 evaluate r2
 t3 <- getCurrentTime
 evaluate r3
 t4 <- getCurrentTime
 --print r1
 print (diffUTCTime t2 t1)
 print (diffUTCTime t3 t2)
 print (diffUTCTime t4 t3)

--0.000631s
--0.000851s
--0.000466s

main  = do
 args <- getArgs
 --g0 <- readCSV "titanic.csv"
 --let (m,g) = trf g0
 --writeFile "m.dat" (show m)
 --writeFile "g.dat" (show g)
 fm <- readFile "m.dat"
 let m = read fm :: Map String Int
 fg <- readFile "g.dat"
 let g = read fg :: Graph Int Int
 let q1 = gmap (get m,get m) $ query (args!!0)
 let r1 = match0 q1 g
 print r1

-- time (./Main "(?x Survived 1)")
--real	0m0.352s
--user	0m0.340s
--sys	0m0.008s
