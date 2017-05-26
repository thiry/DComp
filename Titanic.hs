import Text.CSV -- cabal install csv
import Data.List

import DB

main = do
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

sample1 = do
 file <- readFile "titanic1.dhs"
 let db = read file :: Graph
 let r = answer [("1","X","Y")] db []
 print (r)
