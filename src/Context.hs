module Context where

import Data.List (nub)

type Context = [(String,String)]
ctx = []

has :: String -> Context -> Bool
has x []         = False
has x ((k,v):cs) = if (x==k) then True else has x cs

get :: String -> Context -> String
get x ((k,v):cs) = if (x==k) then v else get x cs

put :: String -> String -> Context -> Context
put k v cs = (k,v):cs

---

-- type Context' = [([String],[[String]])]

trans :: Eq a => [(a, b)] -> [(a, [b])]
trans cs = cs'
 where dom = nub (map fst cs)
       cs' = map (\k -> (k,map snd (filter ((==k).fst) cs))) dom

c = [("x","v1"),("x","v2"),("x","v3"),("y","v4")]

