import Text.CSV -- cabal install csv
import Data.List

import DB2 --import DB
import Measure

step1 = do -- transform csv into graph and split in 2
 content <- parseCSVFromFile "titanic.csv"
 let Right tabl = content
 let headers    = head tabl
 let values     = init (tail tabl) -- 895*12
 let properties = map (\v -> zip headers v) values
 let transform ((_,s):ps) = map (\(p,v) -> (s,p,v)) ps
 let graph      = concat (map transform properties)
 let grph       = filter (\(_,_,x) -> x/="") graph
 -- print (length grph) -- 8935
 writeFile "titanic.dhs" (show grph)
 let n = div (length grph) 2
 writeFile "titanic1.dhs" (show (take n grph))
 writeFile "titanic2.dhs" (show (drop n grph))

step2 = do -- sample query and dot vizualization
 file <- readFile "titanic1.dhs"
 let db = trf (read file) :: Graph -- read file :: Graph
-- display "database" (take 300 db) -- 100
 let query = [("?X","Sex","male"),("?X","Survived","1")]
 let r = answer query db []
-- display "query" query
 print (r)

step3 = do -- compute performance
 file <- readFile "titanic1.dhs"
 let db = trf (read file) :: Graph -- read file :: Graph
 let query = [("?X","Sex","male"),("?X","Survived","1")]
 p <- perf (\(q,d,c) -> answer q d c) (query,db,[])
 print p -- 0.33s
 
step3b = do -- more simple query
 file <- readFile "titanic1.dhs"
 let db = trf (read file) :: Graph -- read file :: Graph
 let query = [("1","?X","?Y")]
 p <- perf (\(q,d,c) -> answer q d c) (query,db,[])
 print p -- 0.18s
 --print (answer query db [])
