module Titanic where 

import Text.CSV -- cabal install csv
import Data.List

import DB --import DB2
import Measure

cvsToDB = do -- transform csv into graph and split in 2
 content <- parseCSVFromFile "titanic.csv"
 let Right tabl = content
 let headers    = head tabl
 let values     = init (tail tabl) -- 895*12
 let properties = map (\v -> zip headers v) values
 let transform ((_,s):ps) = map (\(p,v) -> (s,p,v)) ps
 let graph      = concat (map transform properties)
 let grph       = filter (\(_,_,x) -> x/="") graph
 writeFile "titanic.dhs" (show grph)
 let n = 4469 -- div (length grph) 2
 writeFile "titanic1.dhs"   (show (take 12 grph))
 writeFile "titanic10.dhs"  (show (take 120 grph))
 writeFile "titanic100.dhs" (show (take 1200 grph))
 writeFile "titanica.dhs"   (show (take n grph))
 writeFile "titanicb.dhs"   (show (drop n grph))

run query file = do -- compute performance
 content <- readFile file
 let db = read content :: Graph -- trf (read file) :: Graph
 p <- perf (\(q,d,c) -> answer q d c) (query,db,[])
 putStrLn ("In "++(show $ fst p)++" with "++file) -- 0.33s

