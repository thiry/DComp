module Main where

import Titanic
import Query
import Dist
import DBacc
import qualified DB2 as D
import qualified Dist2 as Ds
--import qualified Mongoo as M

import Control.Concurrent (forkIO,threadDelay)
import System.Environment (getArgs)
import Network

-- Usage: ghc Main.hs && ./Main
-- Then:
-- ./Main "slave" "titanica.dhs" "9020" &
-- ./Main "slave" "titanicb.dhs" "9030" &
-- ./Main "master" "[(\"localhost\","9020"),(\"localhost\","9030")]"  "(?X Sex male) and (?X Survived 1)"
-- ./Main master "(?X Sex male) and (?X Survived 1)"

demo p qry_ = do
 let qry = query qry_
 putStrLn ("Query is: "++qry_)
 sequence_ (map (\file -> run qry file) ["titanic1.dhs","titanic10.dhs","titanic100.dhs","titanica.dhs","titanic.dhs"])
 putStrLn "With 2 computers"
 forkIO (slave "titanica.dhs" p)
 forkIO (slave "titanicb.dhs" (p+5))
 threadDelay 10000
 master' [("localhost",p),("localhost",p+5)] (show qry)
 putStrLn "With new data structure and whole db"
 sequence_ (map (\file -> run' qry file) ["ntitanica.dhs","ntitanic.dhs"])
 putStrLn "With 2 computers"
 forkIO (Ds.slave "ntitanica.dhs" (p+10))
 forkIO (Ds.slave "ntitanicb.dhs" (p+15))
 threadDelay 10000
 Ds.master' [("localhost",p+10),("localhost",p+15)] (show qry)
 putStrLn "---"

main = do
 args <- getArgs
 case args of
  ["slave", file, port] -> slave file (read port :: PortNumber)
  ["master", hosts, q]  -> 
   let hs = read hosts :: [(String,PortNumber)] in master'' hs (show $ query q)
  ["master", q]  -> do 
    c <- readFile "config.txt"
    let hs = read c :: [(String,PortNumber)]
    master'' hs (show $ query q)
  _ -> do cvsToDB
          sample <- readFile "titanic10.dhs"
          let db = read sample :: Graph
          display "partial-db" db
          display "sample-query" (query "(?X Sex male) and (?X Survived 1)")
          putStrLn "---\nSee 'partial.png' for an extract of the database\n---"
          demo 9250 "(?X Sex male)"
          demo 9300 "(?X Sex male) and (?X Survived 1)"
          demo 9350 "(?X ?Y ?Z)"
          putStrLn "With MongoDB and full db"
          --M.run "(?X Sex male)" M.query1
          --M.run "(?X ?Y ?Z)" M.query3

