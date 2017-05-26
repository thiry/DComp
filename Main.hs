module Main where

import Titanic
import Query
import Dist
import DB

import Control.Concurrent (forkIO,threadDelay)
import System.Environment (getArgs)
import Network

qry_ = "(?X Sex male) and (?X Survived 1)"
qry = query qry_

demo = do
 cvsToDB
 sample <- readFile "titanic10.dhs"
 let db = read sample :: Graph
 display "partial" db
 display "query" qry
 putStrLn "See 'partial.png' for an extract of the database"
 putStrLn ("Query is: "++qry_)
 sequence_ (map (\file -> run qry file) ["titanic1.dhs","titanic10.dhs","titanic100.dhs","titanic.dhs"])
 putStrLn "With 2 computers"
 forkIO (slave "titanica.dhs" 9000)
 forkIO (slave "titanicb.dhs" 9010)
 threadDelay 10000
 master' [("localhost",9000),("localhost",9010)] (show qry)

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
  _ -> demo

-- Usage: ghc Main.hs && ./Main
-- Then:
-- ./Main "slave" "titanica.dhs" "9020" &
-- ./Main "slave" "titanicb.dhs" "9030" &
-- ./Main "master" "[(\"localhost\","9020"),(\"localhost\","9030")]"  "(?X Sex male) and (?X Survived 1)"
-- ./Main master "(?X Sex male) and (?X Survived 1)"
