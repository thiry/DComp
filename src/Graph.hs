module Graph where

import Data.List (nub)

type Graph a b = [(a,b,a)]

g :: Graph String String
g = [("a","b","c"),("a","d","e"),("c","f","e")]

dom :: Eq a => Graph a a -> [a]
dom = nub.lay
 where lay []          = []
       lay ((x,y,z):g) = x:y:z:(lay g)

r1 = dom g

gmap :: (a->c,b->d) -> Graph a b -> Graph c d
gmap (fa,fb) = map (\(x,y,z) -> (fa x,fb y,fa z))

type Map a b = [(a,b)]

m :: Map String Int
m = zip r1 [1..]

get :: Eq a => Map a b -> a -> b
get ((k,v):kv) x = if x==k then v else get kv x

g2 :: Graph Int Int
g2 = gmap (get m,get m) g

trf g = (m,g')
 where m  = zip (dom g) [1..]
       g' = gmap (get m,get m) g

q = ("a","?x","c")

tmap f (x,y,z) = (f x,f y,f z)

r2 = tmap (get m) q

