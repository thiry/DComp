module Main where

-- sed 's/^/> /' Main.hs > Main.lhs
-- awk -f bird2code.awk Main.lhs > Main.tex && pdflatex Main.tex

import DB
import Dist
import Measure

import Network -- (PortID(PortNumber),listenOn,accept,connectTo)
import System.Environment (getArgs)

-- Usage: ghc Main.hs && ./Main
-- Then:
-- ./Main "slave" "db.txt" "9000" &
-- ./Main "slave" "db.txt" "9090" &
-- ./Main "master" "[(\"localhost\","9000"),(\"localhost\","9090")]"  "[(\"X\",\"hasskill\",\"computerScience\"),(\"X\",\"workfor\",\"ensisa\")]"
------------------------------------------------------

main = do
 mkdb
 args <- getArgs
 case args of
  ["slave", file, port]   -> slave file (read port :: PortNumber)
  ["master", hosts, query] -> 
   let hs = read hosts :: [(String,PortNumber)] in master' hs query
  _ -> main0

main0 = do 
 display "db" db
 display "query" query
 putStrLn "Having a graph database (see db.png) and a query (see query.png)"
 let r = map snd (answer query db ctx)
 p <- perf (\(q,d,c) -> answer q d c) (query,db,ctx)
 putStrLn ("The program returns "++(show r)++" in "++(show p))
 putStrLn "By multiplying the db size by 10,100,1000... the execution time becomes:"
 performances
 putStrLn ("By spliting db and using networking, the executions times are:")
 demo "db5.txt" 9100
 demo "db50.txt" 9200
 demo "db500.txt" 9300
 demo "db5000.txt" 9400

-- Having a graph database (see db.png) and a query (see query.png)
-- The program returns [[("X","laurent")]] in 0.000053s
-- By multiplying the db size by 10,100,1000 and 10000, execution times become:
-- 0.009335s
-- 0.232419s
-- 21.851058s
-- 2237.610898s
-- ...
-- By spliting db and using networking, execution times are:
-- [("X","laurent")] in 0.00457s
-- [("X","laurent")] in 0.160813s
-- [("X","laurent")] in 17.411069s

-- Note: by using ghc, and not ghci, time is divided by 10 
--       by adding -O2, time is also divided by 10 !


mkdb = do
 writeFile "db.txt" (show db)
 writeFile "db5.txt" (show (loop 5 db))
 writeFile "db50.txt" (show (loop 50 db))
 writeFile "db500.txt" (show (loop 500 db))
 writeFile "db5000.txt" (show (loop 5000 db))

dataset = map (\db -> (query,db,ctx)) dbs
dbs = map (\n -> loop n db) [10,100,1000,10000]
loop 1 db = db
loop n db = db++(loop (n-1) db)


-- writeFile "db50.txt" (show $ loop 50 db)

performances = sequence_ (map (>>= print) (perfs (\(q,d,c) -> last (answer q d c)) dataset))
-- Note: "last" is use to force lazy evaluation

savedb db file = writeFile file (show db)

------------------------------------------------------
-- Automatic splitting and deployement (prog trans)
------------------------------------------------------
