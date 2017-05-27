module Titanic where 

import Text.CSV -- cabal install csv
import Data.List

import DB
import qualified DB2 as D
import Measure

cvsToDB = do -- transform csv into graph and split in 2
 content <- parseCSVFromFile "titanic.csv"
 let Right tabl = content
 let headers    = head tabl
 let values     = init (tail tabl) -- 895*12
 let properties = map (\v -> zip headers v) values
 let transform ((_,s):ps) = map (\(p,v) -> (s,p,v)) ps
 let graph      = concat (map transform properties)
 let db         = filter (\(_,_,x) -> x/="") graph
 let db1        = take 12 db
 let db10       = take 120 db
 let db100      = take 1200 db
 let n = 4469   -- split
 let dba        = take n db
 let dbb        = drop n db
 writeFile "titanic.dhs"    (show db)
 writeFile "titanic1.dhs"   (show db1)
 writeFile "titanic10.dhs"  (show db10)
 writeFile "titanic100.dhs" (show db100)
 writeFile "titanica.dhs"   (show dba)
 writeFile "titanicb.dhs"   (show dbb)
 let db'        = D.trf db -- change data structure
 let m          = div (length db') 2
 let dba'       = take m db'
 let dbb'       = drop m db'
 writeFile "ntitanic.dhs"   (show db')
 writeFile "ntitanica.dhs"  (show dba')
 writeFile "ntitanicb.dhs"  (show dbb')

run query file = do -- compute performance
 content <- readFile file
 let db = read content :: Graph -- trf (read file) :: Graph
 p <- perf (\(q,d,c) -> answer q d c) (query,db,[])
 putStrLn ("In "++(show $ fst p)++" with "++file) -- 0.33s

run' query file = do -- compute performance
 content <- readFile file
 let db = read content :: D.Graph -- trf (read file) :: Graph
 p <- perf (\(q,d,c) -> D.answer q d c) (query,db,[])
 putStrLn ("In "++(show $ fst p)++" with "++file) -- 0.33s
