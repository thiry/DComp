module Context where

type Context = [(String,String)]
ctx = []

has :: String -> Context -> Bool
has x []         = False
has x ((k,v):cs) = if (x==k) then True else has x cs

get :: String -> Context -> String
get x ((k,v):cs) = if (x==k) then v else get x cs

put :: String -> String -> Context -> Context
put k v cs = (k,v):cs